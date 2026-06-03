(ns katzen.uwd
  "Undirected wiring diagrams (UWDs) as ACSets on SchUWD.

   A UWD captures a relational composition pattern: a set of *boxes* each
   with a finite set of *ports*, plus a set of *junctions* (the equivalence
   classes of ports that get identified), and a set of *outer-ports* that
   are the composite diagram's own interface. Both ports and outer-ports
   are attached to junctions via the `:junction` / `:outer-junction` homs;
   the connectivity pattern is exactly that assignment.

   This is the data shape Catlab.jl uses to represent the typed term
   `R₁(x, y) ∧ R₂(y, z) ∧ R₃(z, x)` — three boxes, three ports each,
   junctions {x, y, z}, no outer-ports. The same data shape underlies
   Catlab's UWD-based composition for AlgebraicPetri, AlgebraicDynamics,
   etc.

   This v1 port deliberately omits the `:Name` attr Catlab carries on
   Box — we identify boxes by part-id only. Names can be added as an
   attr-typed schema variant if downstream needs them."
  (:require [katzen.acset :as a]))

;; ============================================================================
;; SchUWD
;; ============================================================================

(def SchUWD
  "Schema of (untyped) undirected wiring diagrams.

   Objects:
     :Box        — the operations being composed
     :Port       — each box's interface point
     :Junction   — the equivalence class a port is attached to
     :OuterPort  — the composite diagram's own outer interface

   Homs:
     :port-box       Port → Box        which box owns each port
     :junction       Port → Junction   the junction each port attaches to
     :outer-junction OuterPort → Junction"
  {:name       'SchUWD
   :objects    [:Box :Port :Junction :OuterPort]
   :homs       [{:name :port-box       :dom :Port      :codom :Box}
                {:name :junction       :dom :Port      :codom :Junction}
                {:name :outer-junction :dom :OuterPort :codom :Junction}]
   :attr-types []
   :attrs      []})

;; ============================================================================
;; Constructors
;; ============================================================================

(defn uwd
  "Empty UWD."
  []
  (a/vector-acset SchUWD))

(defn add-junction
  "Allocate a fresh junction. Returns [new-uwd j-id]."
  [d]
  (a/add-part d :Junction))

(defn add-junctions
  "Allocate n fresh junctions. Returns [new-uwd j-ids]."
  [d n]
  (a/add-parts d :Junction n))

(defn add-box
  "Allocate a fresh box. Returns [new-uwd b-id]."
  [d]
  (a/add-part d :Box))

(defn add-port
  "Attach a port to box `b` connecting to junction `j`. Returns
   [new-uwd port-id]."
  [d b j]
  (let [[d p] (a/add-part d :Port)
        d (-> d
              (a/set-subpart :port-box p b)
              (a/set-subpart :junction p j))]
    [d p]))

(defn add-box-with-ports
  "Add a box plus one port for each junction in `junction-ids`.
   Returns [new-uwd b-id port-ids]."
  [d junction-ids]
  (let [[d b] (add-box d)
        [d ports] (reduce
                   (fn [[d ps] j]
                     (let [[d p] (add-port d b j)]
                       [d (conj ps p)]))
                   [d []]
                   junction-ids)]
    [d b ports]))

(defn add-outer-port
  "Add an outer port attached to junction `j`. Returns [new-uwd op-id]."
  [d j]
  (let [[d op] (a/add-part d :OuterPort)]
    [(a/set-subpart d :outer-junction op j) op]))

;; ============================================================================
;; Accessors mirroring the ACSet protocol with UWD-flavored names
;; ============================================================================

(defn boxes        [d] (a/parts d :Box))
(defn ports        [d] (a/parts d :Port))
(defn junctions    [d] (a/parts d :Junction))
(defn outer-ports  [d] (a/parts d :OuterPort))

(defn nboxes       [d] (a/nparts d :Box))
(defn nports       [d] (a/nparts d :Port))
(defn njunctions   [d] (a/nparts d :Junction))
(defn nouter-ports [d] (a/nparts d :OuterPort))

(defn port-box [d p] (a/subpart d :port-box p))
(defn port-junction [d p] (a/subpart d :junction p))
(defn outer-junction [d op] (a/subpart d :outer-junction op))

(defn box-ports
  "Sorted vec of port-ids belonging to box b."
  [d b]
  (vec (a/incident d :port-box b)))

(defn box-junctions
  "Sorted vec of junction-ids that box b's ports attach to, in port order."
  [d b]
  (mapv (fn [p] (port-junction d p)) (box-ports d b)))

(defn junction-ports
  "Sorted vec of port-ids attached to junction j."
  [d j]
  (vec (a/incident d :junction j)))

;; ============================================================================
;; Relations algebra — the canonical oapply instance
;; ============================================================================
;;
;; A *relation* on a tuple of junction types is a set of tuples. For this
;; v1 we assume every junction has the same type, a single FinSet of
;; cardinality `n`. A box's value is a set of tuples, one tuple per port
;; in port-order. A junction value is an element of the FinSet. The
;; composite relation holds for a junction-assignment `js` iff every box
;; B's port-tuple `(js[jₚ₁], …, js[jₚₖ])` is in B's value.
;;
;; This is the relations algebra; it's the trivial case that exercises
;; oapply wiring without any algebra-specific complexity.

(defn- box-port-tuple
  "Given a junction-assignment vector indexed by junction-id (0-based on the
   ACSet's part-ids minus 1) and a box's port-junctions, build the tuple to
   look up in the box's relation."
  [j-assignment box-js]
  (mapv (fn [j] (get j-assignment (dec j))) box-js))

(defn oapply-relations
  "Apply the relations-algebra oapply to a UWD `d` with `box-values` a map
   from box-id → set of tuples (each tuple being a vector of junction values).

   `type-size` is the cardinality of the (common) junction type — the
   range of values each junction can take, conventionally 0..type-size-1.

   Returns a set of tuples: each tuple is a vector of length `(njunctions d)`
   in junction-id order. A tuple is included iff projecting it through every
   box's ports yields a tuple in that box's relation."
  [d box-values type-size]
  (let [nj  (njunctions d)
        all-boxes (boxes d)
        box-ports (into {} (for [b all-boxes] [b (box-junctions d b)]))]
    (->> (apply concat
                (reductions
                 (fn [tuples _]
                   (mapcat (fn [t] (map #(conj t %) (range type-size))) tuples))
                 [[]]
                 (range nj)))
         (filter #(= nj (count %)))
         (filter
          (fn [t]
            (every?
             (fn [b]
               (let [box-tuple (box-port-tuple t (get box-ports b))]
                 (contains? (get box-values b #{}) box-tuple)))
             all-boxes)))
         set)))
