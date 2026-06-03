(ns katzen.cpg
  "Circular port graphs (CPGs) — the AlgebraicDynamics interaction-network
   composition pattern.

   In a CPG, each box owns a set of interaction `Port`s, and `Edge`s
   carry signals between ports. Unlike DWDs, ports here have *dual*
   semantics: each port is simultaneously a box input (it can receive
   from edges targeting it) AND a box output (it can be a source).
   The box machine must therefore satisfy `n-inputs = n-outputs = n-ports`.

   Open CPGs additionally have `OuterPort`s connected to internal box
   ports via `Conn` connections, supplying the composite's external
   interface.

   Implementation: we represent CPGs as their own ACSet schema for the
   schema-level operations (validation, visualization, migration), but
   the dynamics composition is delegated to `katzen.dwd.dynamics/oapply-dwd`
   via `cpg->dwd` — each CPG port becomes a paired DWD InPort/OutPort,
   each CPG edge becomes a DWD wire, and OuterPort/Conn pairs become
   outer-input wires from outer-input ports.

   The result of `oapply-cpg` is therefore an honest-to-protocol
   Machine, immediately usable with the standard compile-rhs driver
   (when n-inputs = 0)."
  (:require [katzen.acset :as a]
            [katzen.dwd :as dwd]
            [katzen.dwd.dynamics :as mach]))

;; ============================================================================
;; SchCPG
;; ============================================================================

(def SchCPG
  "Open circular port graph schema."
  {:name       'SchCPG
   :objects    [:Box :Port :Edge :OuterPort :Conn]
   :homs       [{:name :port-box   :dom :Port  :codom :Box}
                {:name :src        :dom :Edge  :codom :Port}
                {:name :tgt        :dom :Edge  :codom :Port}
                {:name :outer-port :dom :Conn  :codom :OuterPort}
                {:name :inner-port :dom :Conn  :codom :Port}]
   :attr-types []
   :attrs      []})

;; ============================================================================
;; Constructors
;; ============================================================================

(defn cpg [] (a/vector-acset SchCPG))

(defn add-cpg-box
  "Allocate a box. Returns [new-cpg b-id]."
  [g] (a/add-part g :Box))

(defn add-cpg-port
  "Add a port to box `b`. Returns [new-cpg p-id]."
  [g b]
  (let [[g p] (a/add-part g :Port)]
    [(a/set-subpart g :port-box p b) p]))

(defn add-cpg-box-with-ports
  "Allocate a box with `n` ports. Returns [new-cpg b-id port-ids]."
  [g n]
  (let [[g b] (add-cpg-box g)
        [g ports] (reduce (fn [[g ps] _]
                            (let [[g p] (add-cpg-port g b)]
                              [g (conj ps p)]))
                          [g []]
                          (range n))]
    [g b ports]))

(defn add-edge
  "Edge from port `src` to port `tgt`. Returns [new-cpg e-id]."
  [g src tgt]
  (let [[g e] (a/add-part g :Edge)]
    [(-> g (a/set-subpart :src e src) (a/set-subpart :tgt e tgt)) e]))

(defn add-outer-port [g]
  (a/add-part g :OuterPort))

(defn add-conn
  "Connect outer-port `op` to inner port `p`. Returns [new-cpg c-id]."
  [g op p]
  (let [[g c] (a/add-part g :Conn)]
    [(-> g (a/set-subpart :outer-port c op)
         (a/set-subpart :inner-port c p)) c]))

;; ============================================================================
;; Accessors
;; ============================================================================

(defn boxes        [g] (a/parts g :Box))
(defn ports        [g] (a/parts g :Port))
(defn edges        [g] (a/parts g :Edge))
(defn outer-ports  [g] (a/parts g :OuterPort))

(defn box-ports
  "Sorted vec of ports on box b."
  [g b]
  (vec (a/incident g :port-box b)))

(defn nports
  "Number of ports on box b."
  [g b]
  (count (box-ports g b)))

(defn edge-src [g e] (a/subpart g :src e))
(defn edge-tgt [g e] (a/subpart g :tgt e))

;; ============================================================================
;; CPG → DWD translation
;; ============================================================================
;;
;; Per the dual-role semantics of CPG ports: each CPG port `p` belonging
;; to box `b` becomes two DWD ports — an InPort and an OutPort, both on
;; the DWD's corresponding box. CPG edges (src-port → tgt-port) become
;; DWD wires from the source's OutPort to the target's InPort.
;;
;; The Conn table on OpenCPortGraph (outer-port → inner-port) becomes
;; input wires (from outer-input ports to box InPorts). Conns from inner
;; ports to outer outputs become output wires.

(defn cpg->dwd
  "Translate a CPG into a DWD plus a port-index map. Returns
   `{:dwd <dwd-acset> :cpg->dwd-box {cpg-box-id → dwd-box-id}
     :port->dwd-in {cpg-port-id → dwd-in-port-id}
     :port->dwd-out {cpg-port-id → dwd-out-port-id}
     :outer-port->dwd-outer-in {cpg-outer-port-id → dwd-outer-in-port-id}}`."
  [g]
  (let [;; Step 1: boxes with paired in/out ports per CPG port.
        init {:dwd (dwd/dwd)
              :cpg->dwd-box {}
              :port->dwd-in {}
              :port->dwd-out {}
              :outer-port->dwd-outer-in {}}
        with-boxes
        (reduce
         (fn [acc cpg-b]
           (let [n (nports g cpg-b)
                 [d dwd-b ips ops] (dwd/add-box-with-ports (:dwd acc) n n)
                 cpg-ps (box-ports g cpg-b)]
             (-> acc
                 (assoc :dwd d)
                 (update :cpg->dwd-box assoc cpg-b dwd-b)
                 (update :port->dwd-in into (zipmap cpg-ps ips))
                 (update :port->dwd-out into (zipmap cpg-ps ops)))))
         init
         (boxes g))
        ;; Step 2: edges become wires src.out-port → tgt.in-port.
        with-edges
        (reduce
         (fn [acc e]
           (let [src-p (edge-src g e)
                 tgt-p (edge-tgt g e)
                 src-op (get (:port->dwd-out acc) src-p)
                 tgt-ip (get (:port->dwd-in  acc) tgt-p)
                 [d _] (dwd/add-box-wire (:dwd acc) src-op tgt-ip)]
             (assoc acc :dwd d)))
         with-boxes
         (edges g))
        ;; Step 3: outer-ports + conns. Each CPG outer-port becomes a
        ;; DWD outer-input port. Conns become input wires from outer
        ;; to inner box InPorts.
        with-outer
        (reduce
         (fn [acc op]
           (let [[d dwd-oin] (dwd/add-outer-in-port (:dwd acc))]
             (-> acc
                 (assoc :dwd d)
                 (update :outer-port->dwd-outer-in assoc op dwd-oin))))
         with-edges
         (outer-ports g))
        with-conns
        (reduce
         (fn [acc c]
           (let [outer (a/subpart g :outer-port c)
                 inner (a/subpart g :inner-port c)
                 dwd-oin (get (:outer-port->dwd-outer-in acc) outer)
                 dwd-ip  (get (:port->dwd-in acc) inner)
                 [d _] (dwd/add-input-wire (:dwd acc) dwd-oin dwd-ip)]
             (assoc acc :dwd d)))
         with-outer
         (a/parts g :Conn))]
    with-conns))

;; ============================================================================
;; oapply-cpg
;; ============================================================================

(defn- box-port-count [g b] (nports g b))

(defn oapply-cpg
  "Compose `box->machine` through the CPG `g`. Each box's machine must
   have `n-inputs = n-outputs = (nports g box)` — each port carries
   one signal in each direction.

   Returns a Machine (per `katzen.dwd.dynamics`). When the CPG has no
   outer-port connections, the returned Machine is closed and works
   directly with `katzen.compile.core/compile-rhs`."
  [g box->machine]
  (let [{:keys [dwd cpg->dwd-box]} (cpg->dwd g)]
    (doseq [b (boxes g)]
      (let [m (get box->machine b)
            n (box-port-count g b)]
        (when-not (and m (= n (:n-inputs m)) (= n (:n-outputs m)))
          (throw (ex-info (str "CPG box " b " requires a machine with n-inputs = n-outputs = " n)
                          {:box b :n-ports n
                           :machine-inputs  (some-> m :n-inputs)
                           :machine-outputs (some-> m :n-outputs)})))))
    (mach/oapply-dwd dwd
                     (into {} (for [[cpg-b dwd-b] cpg->dwd-box]
                                [dwd-b (get box->machine cpg-b)])))))
