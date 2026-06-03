(ns katzen.acset.migration
  "Δ-migration of ACSets along schema morphisms.

   A SchemaMorphism F: C → D is a functor between ACSet schemas determined by:

   - ob-map  : each object of C → an object of D
   - hom-map : each hom f: A → B of C → a path F(f) of D-homs from F(A) to F(B);
               an empty path means the identity on F(A) (which requires F(A)=F(B))
   - attr-map: each attr a: A → T of C → a path of D-morphisms starting at F(A)
               and ending in a D-attr whose codom (an attr-type) matches T

   Given an ACSet X on D, the pullback / contravariant migration
   `(migrate F X)` returns an ACSet Y on C with

     Y(O)     = X(F(O))               (one Y-part of O per X-part of F(O))
     Y(f)(p)  = X(F(f))(M(p))         (where M is the part-bijection per object)

   This is Catlab's `DataMigrationFunctor` for the basic Δ-case. Σ-/Π-style
   migrations (sum and product) are not yet implemented.

   Backends: the target ACSet uses, by default, whatever backend the source X
   does. Pass `:target :vector` or `:target :datahike` to override — useful
   when you want to migrate from a persistent store into a fast in-memory
   structure (or vice versa).

   DatahikeACSet is resolved lazily so this namespace loads without datahike
   on the classpath; only datahike sources or `:target :datahike` invoke it."
  (:require [katzen.acset :as a]))

;; ============================================================================
;; Schema morphism record
;; ============================================================================

(defrecord SchemaMorphism [name dom codom ob-map hom-map attr-map]
  Object
  (toString [_]
    (str "SchemaMorphism(" name ": " (:name dom) " → " (:name codom) ")")))

(defn schema-morphism
  "Construct a SchemaMorphism. Validation deferred to validate-schema-morphism!."
  ([name dom codom ob-map hom-map]
   (schema-morphism name dom codom ob-map hom-map {}))
  ([name dom codom ob-map hom-map attr-map]
   (when-not (a/schema-map? dom)
     (throw (ex-info "schema-morphism: dom must be a schema map" {:got dom})))
   (when-not (a/schema-map? codom)
     (throw (ex-info "schema-morphism: codom must be a schema map" {:got codom})))
   (->SchemaMorphism name dom codom ob-map hom-map attr-map)))

(defn schema-morphism? [x] (instance? SchemaMorphism x))

;; ============================================================================
;; Path resolution and validation
;; ============================================================================

(defn- path-endpoints
  "Walk a path [m1 m2 ... mn] of morphism-names in `codom-schema` starting at
   `start-ob`. Return [end-ob :hom] for a pure-hom path, or [attr-type :attr]
   if the last step is an Attr. Throws if any step is unknown or the chain
   discontinuous."
  [codom-schema start-ob path]
  (loop [cur start-ob, remaining path, ends-with :hom]
    (if (empty? remaining)
      [cur ends-with]
      (let [mname (first remaining)]
        (if-let [[kind {dom :dom codom :codom}] (a/morphism-by-name codom-schema mname)]
          (do
            (when-not (= dom cur)
              (throw (ex-info "Path discontinuous"
                              {:morphism mname :expected-dom cur :actual-dom dom})))
            (recur codom (rest remaining) kind))
          (throw (ex-info "Unknown morphism in path"
                          {:morphism mname :codom-schema (:name codom-schema)})))))))

(defn validate-schema-morphism!
  "Check that F: C → D maps every C-object/hom/attr and that the targets are
   well-formed paths in D. Returns F on success; throws on the first error."
  [F]
  (let [{:keys [name dom codom ob-map hom-map attr-map]} F
        D-objects (set (:objects codom))]
    ;; Every C-object is mapped to a known D-object.
    (doseq [O (:objects dom)]
      (let [FO (get ob-map O)]
        (when-not FO
          (throw (ex-info "SchemaMorphism missing ob-map entry"
                          {:morphism name :object O})))
        (when-not (D-objects FO)
          (throw (ex-info "SchemaMorphism maps to unknown codom object"
                          {:morphism name :object O :target FO})))))
    ;; Every C-hom has a path in D from F(dom) to F(codom).
    (doseq [{f :name f-dom :dom f-codom :codom} (:homs dom)]
      (let [path (get hom-map f)]
        (when (nil? path)
          (throw (ex-info "SchemaMorphism missing hom-map entry"
                          {:morphism name :hom f})))
        (let [F-dom (get ob-map f-dom)
              F-codom (get ob-map f-codom)
              [end kind] (if (empty? path)
                           [F-dom :hom]
                           (path-endpoints codom F-dom path))]
          (when-not (= kind :hom)
            (throw (ex-info "Hom path ends in an Attr; expected Hom"
                            {:morphism name :hom f :path path})))
          (when-not (= end F-codom)
            (throw (ex-info "Hom path does not land on F(codom)"
                            {:morphism name :hom f :path path
                             :expected-end F-codom :actual-end end}))))))
    ;; Every C-attr has a path in D ending in an Attr of matching type.
    (doseq [{a :name a-dom :dom a-codom :codom} (:attrs dom)]
      (let [path (get attr-map a)]
        (when (nil? path)
          (throw (ex-info "SchemaMorphism missing attr-map entry"
                          {:morphism name :attr a})))
        (when (empty? path)
          (throw (ex-info "Attr path must be non-empty (need a final Attr step)"
                          {:morphism name :attr a})))
        (let [F-dom (get ob-map a-dom)
              [end kind] (path-endpoints codom F-dom path)]
          (when-not (= kind :attr)
            (throw (ex-info "Attr path must end in an Attr step"
                            {:morphism name :attr a :path path})))
          ;; end is an attr-type name; check codom attr-types contain it AND
          ;; that it matches the dom attr's declared codom type by name.
          (when-not (= end a-codom)
            (throw (ex-info "Attr path ends at wrong attr-type"
                            {:morphism name :attr a :path path
                             :expected-attr-type a-codom
                             :actual-attr-type end}))))))
    F))

;; ============================================================================
;; Backend dispatch for the target ACSet
;; ============================================================================

(defn- detect-backend
  "Identify the backend of an ACSet. The datahike-backed record carries a
   `:conn` field; the in-memory one doesn't. We dispatch on that rather than
   on the class so this namespace doesn't need datahike loaded."
  [acset]
  (if (and (record? acset) (contains? acset :conn)) :datahike :vector))

(defn- empty-target
  "Allocate an empty ACSet on `schema-data` using the given backend.
   Datahike constructor is resolved lazily."
  [backend schema-data]
  (case backend
    :vector   (a/vector-acset schema-data)
    :datahike (let [ctor (requiring-resolve 'katzen.acset.datahike/datahike-acset)]
                (when-not ctor
                  (throw (ex-info "datahike backend not available — add :datahike alias"
                                  {})))
                (ctor schema-data))
    (throw (ex-info "Unknown backend" {:backend backend}))))

;; ============================================================================
;; Migration
;; ============================================================================

(defn- apply-path
  "Apply a path of D-morphism names to a D-part by repeated subpart lookup.
   Empty path returns `x` unchanged (identity). Returns nil as soon as any
   step is unset — propagates 'partial morphism' through the chain."
  [X path x]
  (reduce (fn [cur mname]
            (if (nil? cur)
              (reduced nil)
              (a/subpart X mname cur)))
          x
          path))

(defn migrate*
  "Like `migrate` but returns `{:result Y :bijection {O {:x->y … :y->x …}}}`,
   exposing the per-object part bijection between source and target ACSets.

   Downstream concept-aware migrations (e.g. PetriDynamics rate pullback)
   use the bijection to translate auxiliary parameter maps keyed by
   source-side part-ids into the same shape on the target side."
  ([F X] (migrate* F X {}))
  ([F X {:keys [target] :or {target :match-source}}]
   (when-not (schema-morphism? F)
     (throw (ex-info "migrate expects a SchemaMorphism" {:got F})))
   (when-not (a/acset? X)
     (throw (ex-info "migrate expects an IACSet" {:got X})))
   (when-not (= (:codom F) (a/schema X))
     (throw (ex-info "Schema mismatch: F's codom must equal X's schema"
                     {:morphism (:name F)
                      :F-codom (:name (:codom F))
                      :X-schema (:name (a/schema X))})))
   (validate-schema-morphism! F)
   (let [C       (:dom F)
         backend (if (= target :match-source)
                   (detect-backend X)
                   target)
         Y0      (empty-target backend C)
         {:keys [ob-map hom-map attr-map]} F
         [Y1 xy-by-ob]
         (reduce
          (fn [[Y acc] O]
            (let [FO         (get ob-map O)
                  xs         (vec (a/parts X FO))
                  [Y' new-ids] (a/add-parts Y O (count xs))]
              [Y' (assoc acc O {:x->y (zipmap xs new-ids)
                                :y->x (zipmap new-ids xs)})]))
          [Y0 {}]
          (:objects C))
         Y2
         (reduce
          (fn [Y {f :name f-dom :dom f-codom :codom}]
            (let [path     (get hom-map f)
                  by-codom (get xy-by-ob f-codom)]
              (reduce-kv
               (fn [Y y-part x-part]
                 (let [x' (apply-path X path x-part)
                       y' (when (some? x') (get (:x->y by-codom) x'))]
                   (if (some? y')
                     (a/set-subpart Y f y-part y')
                     Y)))
               Y
               (:y->x (get xy-by-ob f-dom)))))
          Y1
          (:homs C))
         Y3
         (reduce
          (fn [Y {a :name a-dom :dom}]
            (let [path (get attr-map a)]
              (reduce-kv
               (fn [Y y-part x-part]
                 (let [v (apply-path X path x-part)]
                   (if (some? v)
                     (a/set-subpart Y a y-part v)
                     Y)))
               Y
               (:y->x (get xy-by-ob a-dom)))))
          Y2
          (:attrs C))]
     {:result Y3 :bijection xy-by-ob})))

(defn migrate
  "Δ-migration of X along F. Returns a new ACSet on (dom F).

   Options:
     :target  :match-source | :vector | :datahike   (default :match-source)

   For the bijection between source and target part-ids (used by
   downstream parameter pullbacks), see `migrate*`."
  ([F X] (migrate F X {}))
  ([F X opts] (:result (migrate* F X opts))))

;; ============================================================================
;; Morphism-level migration (Δ_F on ACSet morphisms)
;; ============================================================================
;;
;; Δ_F is a functor C-Set(D) → C-Set(C): not only does it take instances
;; on D to instances on C, it lifts every ACSet morphism φ: X → X' on D
;; to a morphism Δ_F(φ): Δ_F(X) → Δ_F(X') on C. The lifted morphism is
;; built per-object: a Y-part of O corresponds to an X-part of F(O) via
;; the migration bijection, φ on F(O) sends that to an X'-part of F(O),
;; and the inverse bijection sends it to a Y'-part of O.
;;
;; Naturality of Δ_F(φ) in C follows directly from naturality of φ in D
;; — verified concretely in tests, and a sanity check inside
;; `migrate-morphism` could call `morphism/check-natural!` on the result
;; for paranoia. We don't by default to keep the call cheap.

(defn migrate-morphism
  "Lift an ACSet morphism through Δ-migration. Given a SchemaMorphism F
   and a morphism `phi : X → X'` on F's codomain schema, returns an
   ACSet morphism `Δ_F(phi) : Δ_F(X) → Δ_F(X')` on F's domain schema.

   The returned morphism is natural by the categorical theorem (Δ_F
   is a functor); we don't re-verify naturality at runtime here."
  [F phi]
  ;; Inline-require to avoid cyclic dependency between migration and morphism.
  (let [acset-morphism (requiring-resolve 'katzen.acset.morphism/acset-morphism)
        {phi-src :src phi-tgt :tgt phi-comp :components} phi
        x-mig  (migrate* F phi-src)
        x'-mig (migrate* F phi-tgt)
        Y      (:result x-mig)
        Y'     (:result x'-mig)
        x-bij  (:bijection x-mig)
        x'-bij (:bijection x'-mig)
        C      (:dom F)
        ob-map (:ob-map F)
        components'
        (into {}
              (for [O (:objects C)
                    :let [F-O    (get ob-map O)
                          y->x   (:y->x (get x-bij O))
                          x'->y' (:x->y (get x'-bij O))
                          phi-F-O (get phi-comp F-O {})]]
                [O (into {} (for [[y x] y->x
                                  :let [phi-x (get phi-F-O x)
                                        y'    (get x'->y' phi-x)]
                                  :when (some? y')]
                              [y y']))]))]
    (acset-morphism Y Y' components')))

;; ============================================================================
;; Canonical schema morphisms on SchGraph
;; ============================================================================

(def IdGraph
  "Identity migration on SchGraph: Δ_id(G) ≅ G."
  (schema-morphism 'IdGraph a/SchGraph a/SchGraph
                   {:V :V, :E :E}
                   {:src [:src], :tgt [:tgt]}))

(def OpGraph
  "Edge-reversal migration on SchGraph: swap src and tgt. Δ_op(G) is G with
   every edge reversed. This is the schema-level dual of Catlab's
   `dual_graph` data migration."
  (schema-morphism 'OpGraph a/SchGraph a/SchGraph
                   {:V :V, :E :E}
                   {:src [:tgt], :tgt [:src]}))
