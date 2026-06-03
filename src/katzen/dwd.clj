(ns katzen.dwd
  "Directed wiring diagrams (DWDs) as ACSets on SchDWD.

   A DWD captures a *directed* composition pattern with typed I/O at the
   diagram boundary: every box has input ports and output ports, every
   wire has a designated source and target, and the diagram itself has
   outer-input and outer-output ports — the composite's external
   interface.

   Wires come in three shapes:
     - box output → box input      (most wires, internal routing)
     - outer input → box input     (pass an outer input into a box)
     - box output → outer output   (expose a box output at the boundary)

   We model wires as a single `:Wire` object with four possible source/
   target hom slots — exactly one source slot and one target slot are
   set per wire. Helper constructors hide that and surface
   `add-box-wire`, `add-input-wire`, `add-output-wire`.

   The data shape matches Catlab.jl's `WiringDiagram`. Together with
   `katzen.dwd.dynamics` it underlies our AlgebraicDynamics analogue:
   the `Machine` record + `oapply-dwd` composition operator."
  (:require [katzen.acset :as a]))

;; ============================================================================
;; SchDWD
;; ============================================================================

(def SchDWD
  "Schema of directed wiring diagrams."
  {:name       'SchDWD
   :objects    [:Box :InPort :OutPort :OuterInPort :OuterOutPort :Wire]
   :homs       [{:name :ip-box        :dom :InPort  :codom :Box}
                {:name :op-box        :dom :OutPort :codom :Box}
                ;; Wire endpoints. Each wire has exactly one source slot
                ;; and one target slot set; the other two stay nil.
                {:name :src-op        :dom :Wire :codom :OutPort}
                {:name :src-outer-in  :dom :Wire :codom :OuterInPort}
                {:name :tgt-ip        :dom :Wire :codom :InPort}
                {:name :tgt-outer-out :dom :Wire :codom :OuterOutPort}]
   :attr-types []
   :attrs      []})

;; ============================================================================
;; Construction
;; ============================================================================

(defn dwd [] (a/vector-acset SchDWD))

(defn add-box
  "Allocate a fresh box. Returns [new-dwd b-id]."
  [d] (a/add-part d :Box))

(defn add-in-port
  "Add an input port to box b. Returns [new-dwd ip-id]."
  [d b]
  (let [[d p] (a/add-part d :InPort)]
    [(a/set-subpart d :ip-box p b) p]))

(defn add-out-port
  "Add an output port to box b. Returns [new-dwd op-id]."
  [d b]
  (let [[d p] (a/add-part d :OutPort)]
    [(a/set-subpart d :op-box p b) p]))

(defn add-box-with-ports
  "Add a box with `n-in` input ports and `n-out` output ports. Returns
   [new-dwd b ip-ids op-ids]."
  [d n-in n-out]
  (let [[d b] (add-box d)
        [d ips] (reduce (fn [[d ps] _]
                          (let [[d p] (add-in-port d b)]
                            [d (conj ps p)]))
                        [d []]
                        (range n-in))
        [d ops] (reduce (fn [[d ps] _]
                          (let [[d p] (add-out-port d b)]
                            [d (conj ps p)]))
                        [d []]
                        (range n-out))]
    [d b ips ops]))

(defn add-outer-in-port
  "Add an outer input port. Returns [new-dwd op-id]."
  [d]
  (a/add-part d :OuterInPort))

(defn add-outer-out-port
  "Add an outer output port. Returns [new-dwd op-id]."
  [d]
  (a/add-part d :OuterOutPort))

(defn add-box-wire
  "Connect box output port `src-op` to box input port `tgt-ip`.
   Returns [new-dwd w-id]."
  [d src-op tgt-ip]
  (let [[d w] (a/add-part d :Wire)]
    [(-> d (a/set-subpart :src-op w src-op)
           (a/set-subpart :tgt-ip w tgt-ip)) w]))

(defn add-input-wire
  "Connect outer-input port `src-oi` to box input port `tgt-ip` —
   passes the outer input into the box. Returns [new-dwd w-id]."
  [d src-oi tgt-ip]
  (let [[d w] (a/add-part d :Wire)]
    [(-> d (a/set-subpart :src-outer-in w src-oi)
           (a/set-subpart :tgt-ip w tgt-ip)) w]))

(defn add-output-wire
  "Connect box output port `src-op` to outer-output port `tgt-oo` —
   exposes the box output at the boundary. Returns [new-dwd w-id]."
  [d src-op tgt-oo]
  (let [[d w] (a/add-part d :Wire)]
    [(-> d (a/set-subpart :src-op w src-op)
           (a/set-subpart :tgt-outer-out w tgt-oo)) w]))

;; ============================================================================
;; Accessors
;; ============================================================================

(defn boxes          [d] (a/parts d :Box))
(defn in-ports       [d] (a/parts d :InPort))
(defn out-ports      [d] (a/parts d :OutPort))
(defn outer-in-ports [d] (a/parts d :OuterInPort))
(defn outer-out-ports [d] (a/parts d :OuterOutPort))
(defn wires          [d] (a/parts d :Wire))

(defn box-in-ports
  "Vec of input port ids on box b, in part-id order."
  [d b]
  (vec (a/incident d :ip-box b)))

(defn box-out-ports
  "Vec of output port ids on box b, in part-id order."
  [d b]
  (vec (a/incident d :op-box b)))

(defn wire-source
  "Return the wire's source as a tagged pair: [:out-port op] or
   [:outer-in oi]."
  [d w]
  (let [op (a/subpart d :src-op w)
        oi (a/subpart d :src-outer-in w)]
    (cond
      op [:out-port op]
      oi [:outer-in oi]
      :else nil)))

(defn wire-target
  "Return the wire's target as a tagged pair: [:in-port ip] or
   [:outer-out oo]."
  [d w]
  (let [ip (a/subpart d :tgt-ip w)
        oo (a/subpart d :tgt-outer-out w)]
    (cond
      ip [:in-port ip]
      oo [:outer-out oo]
      :else nil)))

(defn wires-into-in-port
  "Wires whose target is `ip` (a box's input port)."
  [d ip]
  (a/incident d :tgt-ip ip))

(defn wires-into-outer-out
  "Wires whose target is `oo` (an outer-output port)."
  [d oo]
  (a/incident d :tgt-outer-out oo))
