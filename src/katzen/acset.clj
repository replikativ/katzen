(ns katzen.acset
  "ACSets — attributed C-sets, the relational data structure central to
   applied category theory. Direct port of Catlab.jl's ACSets.jl.

   An ACSet on a schema C is a functor C → Set. Implementation: maps
   each object of C to a sequence of \"parts\" (1-based integer ids) and
   each morphism of C to a function from parts of the domain object to
   parts of the codomain object — or, for Attr morphisms, to typed
   attribute values.

   Multiple backends share the IACSet protocol. This file provides the
   protocol plus a persistent VectorACSet (no extra deps). A datahike-
   backed VectorACSet lives under katzen.acset.datahike (loaded only via
   the :datahike alias).

   Catlab.jl ACSets are mutable in-place; Clojure idiom is persistent,
   so add-part / set-subpart / rem-part all return NEW acsets. The
   semantics are otherwise the same.")

;; ============================================================================
;; Schema representation
;; ============================================================================
;;
;; A schema is a plain Clojure map:
;;
;;   {:name      Symbol
;;    :objects   [Symbol ...]            ;; ordered (preserves declaration order)
;;    :homs      [{:name Sym :dom Sym :codom Sym} ...]
;;    :attr-types [Symbol ...]
;;    :attrs     [{:name Sym :dom Sym :codom Sym} ...]
;;    :equations [{:name Sym? :dom Sym :lhs [Sym ...] :rhs [Sym ...] :codom Sym?} ...]
;;    :axioms    [{:name Sym :ctx [{:name Sym :type Sym} ...] :lhs term :rhs term} ...]}
;;
;; The :dom and :codom for :homs are object names; for :attrs, :codom is
;; an attr-type name.
;;
;; :equations are PATH equations (the ACSets.jl `eqs` idiom): two morphism
;; paths out of object :dom — sequences of hom/attr names applied
;; left-to-right — that must agree on every part. :axioms are the general
;; (non-path) term-equation form. Both are enforced by katzen.acset.check
;; (`check-axioms!`) and consumed by the normalizer; `:equations` desugar into
;; `:axioms` (see katzen.acset.check/path-equation->axiom). The datahike
;; backend ignores both (they are instance constraints, not storage schema).
;;
;; katzen.acset.schemas converts a katzen Schema presentation (a
;; presentation of ThSchema) into this map. Hand-written schemas are
;; also fine for fast iteration.

(defn schema-map?
  "Check if x looks like a schema map."
  [x]
  (and (map? x)
       (every? #(contains? x %) [:objects :homs])))

(defn hom-by-name
  "Look up a hom by its symbolic name. Returns the hom spec or nil."
  [schema hom-name]
  (some #(when (= hom-name (:name %)) %) (:homs schema)))

(defn attr-by-name
  "Look up an attr by its symbolic name. Returns the attr spec or nil."
  [schema attr-name]
  (some #(when (= attr-name (:name %)) %) (:attrs schema)))

(defn morphism-by-name
  "Look up a hom or attr by name. Returns [:hom spec] or [:attr spec] or nil."
  [schema mname]
  (or (some->> (hom-by-name schema mname)  (vector :hom))
      (some->> (attr-by-name schema mname) (vector :attr))))

(defn rename-schema
  "Apply a renaming `m` (old-name → new-name; identity for names absent from
   `m`) to every object, attr-type, and morphism name in `schema` — including
   the dom/codom of homs/attrs and the dom + paths of `:equations`. This is the
   binding functor: it maps an abstract, backend-agnostic canonical schema onto
   a store's concrete idents (e.g. {:title :entity/title :employer
   :entity/employer …}), so one definition can be shared across stores. A pure
   renaming is an iso of schemas, so instances and equations carry over."
  [schema m]
  (let [r #(get m % %)
        rename-arrow (fn [a] (-> a (update :name r) (update :dom r) (update :codom r)))]
    (cond-> schema
      (:name schema)       (update :name r)
      (:objects schema)    (update :objects    #(mapv r %))
      (:attr-types schema) (update :attr-types #(mapv r %))
      (:homs schema)       (update :homs  #(mapv rename-arrow %))
      (:attrs schema)      (update :attrs #(mapv rename-arrow %))
      (:equations schema)
      (update :equations
              #(mapv (fn [e] (cond-> (-> e (update :dom r)
                                         (update :lhs (partial mapv r))
                                         (update :rhs (partial mapv r)))
                               (:codom e) (update :codom r)))
                     %)))))

(defn merge-schema
  "Extend `base` schema with `ext`: the union of their objects, attr-types,
   homs, attrs and equations (objects/attr-types deduped by value; homs/attrs
   deduped by `:name`, base winning). The dual of `rename-schema` for sharing:
   a general base schema (e.g. a generic knowledge graph) is EXTENDED by a
   consumer with its domain-specific morphisms, rather than the domain leaking
   into the base. `(rename-schema (merge-schema base ext) idents)` is the usual
   bind-an-extended-schema flow."
  [base ext]
  (let [by-name (fn [xs]                       ; order-preserving dedup by :name (base wins)
                  (second (reduce (fn [[seen acc] a]
                                    (if (seen (:name a)) [seen acc]
                                        [(conj seen (:name a)) (conj acc a)]))
                                  [#{} []] xs)))
        dedup   (fn [xs] (vec (distinct xs)))]
    (-> base
        (update :objects    #(dedup (concat % (:objects ext))))
        (update :attr-types #(dedup (concat % (:attr-types ext))))
        (update :homs       #(by-name (concat % (:homs ext))))
        (update :attrs      #(by-name (concat % (:attrs ext))))
        (update :equations  #(vec (concat % (:equations ext)))))))

;; ============================================================================
;; IACSet protocol
;; ============================================================================

(defprotocol IACSet
  "Operations on an ACSet. Backends: VectorACSet (default), DatahikeACSet (opt)."

  (-schema [acset]
    "Return the schema this ACSet is over.")

  (-nparts [acset ob]
    "Number of parts of the given object.")

  (-parts [acset ob]
    "Sorted seq of all part-ids for the given object (1-based).")

  (-subpart [acset mname part-id]
    "Value of the given morphism (hom or attr) at part-id.
     For Hom: returns the codom part-id (or nil if unset).
     For Attr: returns the typed value (or nil if unset).")

  (-subpart-all [acset mname]
    "Map from part-id → value for the given morphism.")

  (-incident [acset mname value]
    "Sorted seq of part-ids whose value of `mname` equals `value`.
     Inverse-image lookup. For Hom mname, `value` is a codom part-id;
     for Attr, it's a typed value.")

  (-add-parts [acset ob n]
    "Extend the table for `ob` by n new parts. Returns [new-acset new-ids].")

  (-set-subpart [acset mname part-id value]
    "Set the value of `mname` at `part-id`. Returns new acset.
     No type-checking against the schema; that's the caller's job
     until 3.1f lands a validator.")

  (-rem-part [acset ob part-id]
    "Remove the part. Returns new acset. Outbound morphism values from
     this part are cleared; inbound references are NOT cascaded (the
     caller is responsible — Catlab's `rem_part!` warns about this too)."))

;; ============================================================================
;; Public API (thin wrappers over the protocol)
;; ============================================================================

(defn schema       [a]                  (-schema a))
(defn nparts       [a ob]               (-nparts a ob))
(defn parts        [a ob]               (-parts a ob))
(defn subpart      [a mname part-id]    (-subpart a mname part-id))
(defn subpart-all  [a mname]            (-subpart-all a mname))
(defn incident     [a mname value]      (-incident a mname value))
(defn set-subpart  [a mname part-id v]  (-set-subpart a mname part-id v))
(defn rem-part     [a ob part-id]       (-rem-part a ob part-id))

(defn add-parts
  "Extend table by n parts. Returns [new-acset new-ids]."
  [a ob n]
  (-add-parts a ob n))

(defn add-part
  "Add one part. Returns [new-acset new-part-id]."
  [a ob]
  (let [[a' new-ids] (-add-parts a ob 1)]
    [a' (first new-ids)]))

(defn add-part-with
  "Add one part of `ob` and immediately set its subparts. Returns new acset.
   Useful for the common 'add a row with its values' flow:

     (-> g
         (add-part-with :V {})                       ; v1 = 1
         (add-part-with :V {})                       ; v2 = 2
         (add-part-with :E {:src 1, :tgt 2}))"
  [a ob subparts-map]
  (let [[a' new-id] (add-part a ob)]
    (reduce-kv (fn [acc mname v]
                 (set-subpart acc mname new-id v))
               a'
               subparts-map)))

(defn add-parts-with
  "Add multiple parts of `ob`, each with subparts from `subparts-seq`.
   Returns [new-acset new-ids]."
  [a ob subparts-seq]
  (reduce (fn [[acc ids] sps]
            (let [[acc' new-id] (add-part acc ob)
                  acc' (reduce-kv (fn [a mname v]
                                    (set-subpart a mname new-id v))
                                  acc'
                                  sps)]
              [acc' (conj ids new-id)]))
          [a []]
          subparts-seq))

(defn acset? [x] (satisfies? IACSet x))

;; ============================================================================
;; VectorACSet — persistent in-memory implementation
;; ============================================================================
;;
;; State:
;;   :schema    the schema map (see top of file)
;;   :parts     {ob → #{part-id ...}}        ;; what parts exist
;;   :subparts  {mname → {part-id → value}}  ;; values of each morphism

(declare ->VectorACSet)

(defrecord VectorACSet [schema-data parts-map subparts-map]
  IACSet
  (-schema [_] schema-data)

  (-nparts [_ ob]
    (count (get parts-map ob)))

  (-parts [_ ob]
    (sort (get parts-map ob)))

  (-subpart [_ mname part-id]
    (get-in subparts-map [mname part-id]))

  (-subpart-all [_ mname]
    (get subparts-map mname {}))

  (-incident [_ mname value]
    (->> (get subparts-map mname)
         (keep (fn [[pid v]] (when (= v value) pid)))
         sort))

  (-add-parts [self ob n]
    (let [existing (get parts-map ob)
          next-id  (if (empty? existing) 1 (inc (reduce max existing)))
          new-ids  (vec (range next-id (+ next-id n)))
          new-pm   (update parts-map ob (fnil into #{}) new-ids)]
      [(assoc self :parts-map new-pm) new-ids]))

  (-set-subpart [self mname part-id value]
    (assoc self :subparts-map
           (assoc-in subparts-map [mname part-id] value)))

  (-rem-part [self ob part-id]
    (let [;; Drop this part from its object's part set.
          pm' (update parts-map ob disj part-id)
          ;; Drop any subparts AT this part for morphisms whose dom is ob.
          dom-morphisms (->> (concat (:homs schema-data) (:attrs schema-data))
                             (filter #(= ob (:dom %)))
                             (map :name))
          subm' (reduce (fn [sm mname]
                          (update sm mname dissoc part-id))
                        subparts-map
                        dom-morphisms)]
      (-> self
          (assoc :parts-map pm')
          (assoc :subparts-map subm')))))

(defn vector-acset
  "Create an empty VectorACSet for a schema."
  [schema-data]
  (when-not (schema-map? schema-data)
    (throw (ex-info "vector-acset expects a schema map" {:got schema-data})))
  (->VectorACSet schema-data {} {}))

;; ============================================================================
;; Canonical schema: SchGraph
;; ============================================================================
;;
;; Direct port of Catlab.jl's SchGraph:
;;   @present SchGraph(FreeSchema) begin
;;     V::Ob; E::Ob
;;     src::Hom(E,V); tgt::Hom(E,V)
;;   end
;;
;; Hand-written for now; katzen.acset.schemas will derive these from a
;; katzen Schema presentation in 3.1c.

(def SchGraph
  "The schema of directed multigraphs: vertices V and edges E, with
   source and target morphisms src, tgt : E → V.

   Object and morphism names are keywords throughout the katzen.acset
   API — idiomatic Clojure and what add-part-with's map keys expect."
  {:name       'SchGraph
   :objects    [:V :E]
   :homs       [{:name :src :dom :E :codom :V}
                {:name :tgt :dom :E :codom :V}]
   :attr-types []
   :attrs      []})

(defn graph
  "Create an empty graph (an ACSet on SchGraph)."
  []
  (vector-acset SchGraph))

(defn add-vertex
  "Add one vertex. Returns [new-graph vertex-id]."
  [g]
  (add-part g :V))

(defn add-vertices
  "Add n vertices. Returns [new-graph vertex-ids]."
  [g n]
  (add-parts g :V n))

(defn add-edge
  "Add an edge from src to tgt. Returns [new-graph edge-id]."
  [g s t]
  (let [[g' eid] (add-part g :E)
        g' (-> g'
               (set-subpart :src eid s)
               (set-subpart :tgt eid t))]
    [g' eid]))

(defn nv [g] (nparts g :V))
(defn ne [g] (nparts g :E))
(defn src [g eid] (subpart g :src eid))
(defn tgt [g eid] (subpart g :tgt eid))
(defn vertices [g] (parts g :V))
(defn edges    [g] (parts g :E))

(defn out-edges
  "All edges with source = v."
  [g v]
  (incident g :src v))

(defn in-edges
  "All edges with target = v."
  [g v]
  (incident g :tgt v))
