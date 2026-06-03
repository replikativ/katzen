(ns katzen.dwd.dynamics
  "AlgebraicDynamics Machines over DWDs.

   A `Machine` is a directed open continuous system:

     dynamics : (u, x, t) → du contributions
     readout  : (u, t) → output values

   where `u` is the state vector, `x` is the exogenous input vector
   (one entry per input port), and the readout produces one value per
   output port.

   `oapply-dwd` composes Machines through a DWD. The composite is
   itself a Machine — input ports / output ports of the composite
   correspond to the DWD's outer-input / outer-output ports — so
   composition nests. When the resulting Machine has no inputs
   (`n-inputs = 0`), it implements `katzen.compile.core/RasterCompilable`
   directly and can be handed to the standard compile driver.

   Composition follows AlgebraicDynamics.jl's `induced_dynamics`:
   evaluate every box's readouts, compute each box's inputs as the sum
   of incoming wire sources (box readouts and/or composite-level
   inputs), then call box dynamics with those inputs. We emit the
   whole computation inline into one ftm body so the integration loop
   pays no diagram-traversal cost."
  (:require [katzen.acset :as a]
            [katzen.compile.core :as cc]
            [katzen.compile.expr :as ce]
            [katzen.dwd :as dwd]))

;; ============================================================================
;; Machine
;; ============================================================================

(defrecord Machine
           [n-inputs n-outputs layout
            dynamics-emit   ;; (layout, input-syms) → seq of forms accumulating into du
            readout-emit    ;; (layout, output-syms) → seq of [sym form] let-bindings
            dynamics-clj    ;; (layout) → (fn [du u xs t])
            readout-clj])   ;; (layout) → (fn [u t] → vec of n-outputs doubles)

(defn machine
  "Construct a Machine from a spec map.

   Required keys:
     :state-labels    vector of natural state-vector labels
     :n-inputs        number of input ports
     :n-outputs       number of output ports
     :dynamics-emit   fn (layout, input-syms) → forms accumulating into du
     :readout-emit    fn (layout, output-syms) → [sym form] bindings
     :dynamics-clj    fn (layout) → (fn [^doubles du ^doubles u ^doubles xs t])
     :readout-clj     fn (layout) → (fn [^doubles u t] → vec of doubles)"
  [{:keys [state-labels n-inputs n-outputs
           dynamics-emit readout-emit
           dynamics-clj readout-clj]}]
  (->Machine n-inputs n-outputs
             (cc/state-layout state-labels)
             dynamics-emit readout-emit
             dynamics-clj readout-clj))

(defn machine? [x] (instance? Machine x))

;; ============================================================================
;; raw-machine — friendly constructor from opaque Clojure closures
;; ============================================================================
;;
;; The `machine` ctor above is low-level (you hand-write the four emit/clj
;; fns). `raw-machine` is the faithful analog of AlgebraicDynamics'
;; `ContinuousMachine{T}(ninputs, nstates, noutputs, f, r)`: give it a
;; dynamics closure and a readout closure and it fills the rest in. Like
;; `katzen.ode/raw-field`, opaque closures can't be inlined into straight-
;; line raster code, so a `raw-machine` has NO emit bodies — a composite
;; containing one runs on the Clojure path (`eval-dynamics` / `signal-rhs`,
;; or `compile-clojure-rhs` when closed).

(defn- count-of
  "A spec field that may be a count (int) or a seq of labels → its count."
  [x] (if (integer? x) x (count x)))

(defn raw-machine
  "A directed open system from opaque Clojure closures — the friendly,
   AlgebraicDynamics-style machine constructor.

     (raw-machine
       {:state-labels [:α :q :θ]
        :inputs   1                       ; port count, or a label vector
        :dynamics (fn [u x t] …)           ; u=local state vec, x=input vec
        :outputs  1                        ; default = (count state-labels)
        :readout  (fn [u t] …)})           ; default identity → u

   `:dynamics` returns the derivative vector `u̇` (length = #states); `x`
   carries the values arriving on the input ports, in port order.
   `:readout` returns the output vector `y` (length = #outputs) and is
   **state-only** — it may not look at inputs (that is what keeps wiring
   loop-free). Both default sensibly: readout to identity, `:outputs` to
   the number of states.

   No raster body (the closures are opaque) — compose with `oapply-dwd`
   and run via `eval-dynamics` / `signal-rhs`, or `compile-clojure-rhs`
   once the composite is closed (`n-inputs = 0`)."
  [{:keys [state-labels inputs outputs dynamics readout]}]
  (let [state-labels (vec state-labels)
        n-in   (count-of (or inputs 0))
        n-out  (count-of (or outputs (count state-labels)))
        readout (or readout (fn [u _t] u))
        no-emit (fn [& _]
                  (throw (ex-info "raw-machine has no raster body — its dynamics/readout are opaque closures; use eval-dynamics / signal-rhs (or compile-clojure-rhs when closed)"
                                  {:state-labels state-labels})))]
    (machine
     {:state-labels state-labels
      :n-inputs  n-in
      :n-outputs n-out
      :dynamics-emit no-emit
      :readout-emit  no-emit
      :dynamics-clj
      (fn [layout]
        (let [gslots (mapv #(cc/slot layout %) state-labels)
              n      (count gslots)]
          (fn [^doubles du ^doubles u ^doubles xs t]
            (let [local-u  (mapv (fn [^long g] (aget u g)) gslots)
                  local-du (dynamics local-u (vec xs) t)]
              (dotimes [i n]
                (let [g (long (nth gslots i))]
                  (aset du g (+ (aget du g) (double (nth local-du i))))))))))
      :readout-clj
      (fn [layout]
        (let [gslots (mapv #(cc/slot layout %) state-labels)]
          (fn [^doubles u t]
            (let [local-u (mapv (fn [^long g] (aget u g)) gslots)]
              (vec (readout local-u t))))))})))

;; ============================================================================
;; A Machine with n-inputs = 0 is a closed system — it implements
;; RasterCompilable directly.
;; ============================================================================

(extend-protocol cc/RasterCompilable
  Machine
  (-state-layout [m] (:layout m))
  (-raster-body [m _override]
    (when (pos? (:n-inputs m))
      (throw (ex-info "Machine has n-inputs > 0; can't compile as a closed system. Embed in a DWD that wires its outer-inputs first."
                      {:n-inputs (:n-inputs m)})))
    ((:dynamics-emit m) (:layout m) []))
  (-clojure-body [m _override]
    (when (pos? (:n-inputs m))
      (throw (ex-info "Machine has n-inputs > 0; can't compile as a closed system."
                      {:n-inputs (:n-inputs m)})))
    (let [acc      ((:dynamics-clj m) (:layout m))
          empty-xs (double-array 0)]
      (fn [du u t] (acc du u empty-xs t)))))

;; ============================================================================
;; vector-machine — friendly constructor from a symbolic field + readout
;; ============================================================================
;;
;; The directed sibling of `katzen.ode/vector-field`. Where `raw-machine`
;; carries opaque closures (Clojure path only), `vector-machine` takes a
;; SYMBOLIC field and readout — expressions over the state labels, the
;; input-port labels, and named parameters — and compiles BOTH the raster
;; bodies (fast, inlined under composition) and the Clojure bodies. The
;; readout is **state-only**: a readout expression that names an input
;; label errors (which is exactly the loop-freeness invariant).

(defn vector-machine
  "A directed open system from a symbolic field — fast raster path.

     (vector-machine
       {:state-labels [α q θ]
        :inputs  [c]                         ; input-port labels, in order
        :params  {…}
        :field   {α (+ (* -0.313 α) (* 56.7 q) (* 0.232 c))   ; u̇ per state
                  q (+ (* -0.013 α) (* -0.426 q) (* 0.0203 c))
                  θ (* 56.7 q)}
        :readout [θ]})                        ; one expr per output port

   `:field` maps each state to its time-derivative expression over states,
   input labels and params (a missing state ⇒ 0). `:readout` is a vector of
   output expressions over states and params only (defaults to identity —
   one output per state). Compiles to both numeric paths and composes
   through `oapply-dwd` like any machine."
  [{:keys [state-labels inputs params field readout]}]
  (let [state-labels (vec state-labels)
        inputs       (vec inputs)
        params       (or params {})
        readout      (vec (or readout state-labels))
        known        (set state-labels)
        _ (doseq [k (keys field)]
            (when-not (known k)
              (throw (ex-info "Field key is not a declared state"
                              {:label k :states state-labels}))))
        input-idx    (zipmap inputs (range))
        ;; leaf resolvers, one per (flavour × phase)
        dyn-raster-leaf
        (fn [idx-of input->sym]
          (fn [sym]
            (cond
              (contains? idx-of sym)     (list 'raster.arrays/aget 'u (get idx-of sym))
              (contains? input->sym sym) (get input->sym sym)
              (contains? params sym)     (double (get params sym))
              :else nil)))
        dyn-clj-leaf
        (fn [idx-of]
          (fn [sym]
            (cond
              (contains? idx-of sym)   (list 'clojure.core/aget 'u (get idx-of sym))
              (contains? input-idx sym) (list 'clojure.core/aget 'xs (get input-idx sym))
              (contains? params sym)   (double (get params sym))
              :else nil)))
        ro-leaf-raster
        (fn [idx-of]
          (fn [sym]
            (cond
              (contains? idx-of sym) (list 'raster.arrays/aget 'u (get idx-of sym))
              (contains? params sym) (double (get params sym))
              :else nil)))           ; inputs deliberately absent → readout is state-only
        ro-leaf-clj
        (fn [idx-of]
          (fn [sym]
            (cond
              (contains? idx-of sym) (list 'clojure.core/aget 'u (get idx-of sym))
              (contains? params sym) (double (get params sym))
              :else nil)))]
    (machine
     {:state-labels state-labels
      :n-inputs  (count inputs)
      :n-outputs (count readout)
      :dynamics-emit
      (fn [layout input-syms]
        (let [idx-of    (:index-of layout)
              input->sym (zipmap inputs input-syms)
              leaf      (dyn-raster-leaf idx-of input->sym)]
          (vec
           (for [[label expr] field
                 :let [g (cc/slot layout label)]]
             `(raster.arrays/aset
               ~'du ~g
               (raster.numeric/+ (raster.arrays/aget ~'du ~g)
                                 ~(ce/raster-expr expr leaf)))))))
      :readout-emit
      (fn [layout output-syms]
        (let [leaf (ro-leaf-raster (:index-of layout))]
          (vec
           (map (fn [out-sym expr] [out-sym (ce/raster-expr expr leaf)])
                output-syms readout))))
      :dynamics-clj
      (fn [layout]
        (let [idx-of (:index-of layout)
              leaf   (dyn-clj-leaf idx-of)
              form   `(fn [~(with-meta 'du {:tag 'doubles})
                           ~(with-meta 'u {:tag 'doubles})
                           ~(with-meta 'xs {:tag 'doubles})
                           ~'t]
                        ~@(for [[label expr] field
                                :let [g (cc/slot layout label)]]
                            `(clojure.core/aset
                              ~'du ~g
                              (clojure.core/+ (clojure.core/aget ~'du ~g)
                                              (double ~(ce/clj-expr expr leaf))))))]
          (eval form)))
      :readout-clj
      (fn [layout]
        (let [leaf (ro-leaf-clj (:index-of layout))
              form `(fn [~(with-meta 'u {:tag 'doubles}) ~'t]
                      [~@(for [expr readout]
                           `(double ~(ce/clj-expr expr leaf)))])]
          (eval form)))})))

;; ============================================================================
;; oapply-dwd
;; ============================================================================

(defn- gensym-readout [b op-idx] (gensym (str "ro_b" b "_o" op-idx "_")))
(defn- gensym-input   [b ip-idx] (gensym (str "in_b" b "_i" ip-idx "_")))

(defn- sum-form
  "Sum a vector of source-form expressions; 0.0 if empty."
  [forms]
  (case (count forms)
    0 0.0
    1 (first forms)
    (reduce (fn [a b] `(raster.numeric/+ ~a ~b))
            (first forms)
            (rest forms))))

(defn- wire-source-form
  "Source form for a wire. Box-output wires read a precomputed readout
   symbol from `readout-syms`; outer-input wires read from `outer-in-syms`
   (the composite Machine's input symbols, in outer-in-port order)."
  [d wire box->b-idx readout-syms outer-in-syms]
  (let [[kind ref] (dwd/wire-source d wire)]
    (case kind
      :out-port
      (let [src-b (a/subpart d :op-box ref)
            b-idx (get box->b-idx src-b)
            port-idx (.indexOf ^java.util.List (dwd/box-out-ports d src-b) ref)]
        (get-in readout-syms [b-idx port-idx]))
      :outer-in
      (let [oi-idx (.indexOf ^java.util.List (vec (dwd/outer-in-ports d)) ref)]
        (nth outer-in-syms oi-idx))
      (throw (ex-info "Wire has no source" {:wire wire})))))

(defn oapply-dwd
  "Compose `box->machine` through the DWD `d`. Returns a composite
   `Machine` whose state set is the disjoint union of per-box states.

   The composite has:
     - n-inputs  = number of outer-input  ports of `d`
     - n-outputs = number of outer-output ports of `d`
     - dynamics  = evaluate all box readouts, build each box's inputs
                   from incoming wires (box readouts + outer-inputs),
                   then run box dynamics
     - readout   = sum incoming wires per outer-output port

   When `d` has no outer-input ports, the returned Machine implements
   `RasterCompilable` directly and can be handed to
   `katzen.compile.core/compile-rhs`."
  [d box->machine]
  (let [boxes        (vec (dwd/boxes d))
        outer-ins    (vec (dwd/outer-in-ports d))
        outer-outs   (vec (dwd/outer-out-ports d))
        n-inputs     (count outer-ins)
        n-outputs    (count outer-outs)
        box->b-idx   (zipmap boxes (range))
        per-box      (mapv #(get box->machine %) boxes)
        per-layout   (mapv :layout per-box)
        per-size     (mapv :size per-layout)
        offsets      (vec (reductions + 0 (butlast per-size)))
        total        (reduce + 0 per-size)
        composite-idx
        (into {}
              (for [[b m off] (map vector boxes per-box offsets)
                    [label slot] (:index-of (:layout m))]
                [[b label] (+ off slot)]))
        composite-layout (cc/state-layout total composite-idx)

        ;; Per-emit box-layout factory: rebuild each box's layout from the
        ;; layout argument passed in by the caller. For top-level
        ;; composition the caller passes `composite-layout` itself, so we
        ;; recover the natural offsets. For NESTED composition the caller
        ;; passes a layout whose slots are global to the outer composite,
        ;; and we transparently inherit those positions.
        rebuild-box-layout
        (fn [layout b]
          (let [natural (:layout (get box->machine b))]
            (cc/state-layout
             (:size layout)
             (into {}
                   (for [[label _] (:index-of natural)]
                     [label (cc/slot layout [b label])])))))

        ;; --- Raster emit ---
        ;;
        ;; Both dynamics-emit and readout-emit need per-box readout
        ;; symbols. To avoid re-emitting readouts twice in one body if
        ;; both are spliced together, the symbols themselves are stable
        ;; only within one (call to) emit; the outer DWD that consumes
        ;; this Machine will let-bind them anew. CSE in the compiler
        ;; handles any duplication.

        emit-readout-bindings
        (fn [layout box-readout-syms]
          (vec
           (mapcat
            (fn [b]
              (let [m (get box->machine b)
                    b-idx (get box->b-idx b)
                    syms (nth box-readout-syms b-idx)]
                ((:readout-emit m) (rebuild-box-layout layout b) syms)))
            boxes)))

        emit-box-input-bindings
        (fn [box-readout-syms box-input-syms outer-in-syms]
          (vec
           (mapcat
            (fn [b]
              (let [b-idx (get box->b-idx b)
                    ips (dwd/box-in-ports d b)
                    in-syms (nth box-input-syms b-idx)]
                (for [[ip-idx ip] (map vector (range) ips)]
                  (let [ws (dwd/wires-into-in-port d ip)
                        srcs (mapv #(wire-source-form d % box->b-idx
                                                      box-readout-syms outer-in-syms)
                                   ws)]
                    [(nth in-syms ip-idx) (sum-form srcs)]))))
            boxes)))

        emit-dynamics-blocks
        (fn [layout box-input-syms]
          (mapcat
           (fn [b]
             (let [m (get box->machine b)
                   b-idx (get box->b-idx b)
                   in-syms (nth box-input-syms b-idx)]
               ((:dynamics-emit m) (rebuild-box-layout layout b) in-syms)))
           boxes))

        composite-dynamics-emit
        (fn [layout outer-in-syms]
          (let [box-readout-syms
                (mapv (fn [b]
                        (let [b-idx (get box->b-idx b)
                              m (get box->machine b)]
                          (mapv #(gensym-readout b-idx %) (range (:n-outputs m)))))
                      boxes)
                box-input-syms
                (mapv (fn [b]
                        (let [b-idx (get box->b-idx b)
                              m (get box->machine b)]
                          (mapv #(gensym-input b-idx %) (range (:n-inputs m)))))
                      boxes)
                ro-bindings  (emit-readout-bindings layout box-readout-syms)
                in-bindings  (emit-box-input-bindings box-readout-syms box-input-syms
                                                      outer-in-syms)
                dyn-blocks   (emit-dynamics-blocks layout box-input-syms)]
            [`(let [~@(mapcat identity ro-bindings)
                    ~@(mapcat identity in-bindings)]
                ~@dyn-blocks)]))

        composite-readout-emit
        (fn [layout outer-out-syms]
          (let [box-readout-syms
                (mapv (fn [b]
                        (let [b-idx (get box->b-idx b)
                              m (get box->machine b)]
                          (mapv #(gensym-readout b-idx %) (range (:n-outputs m)))))
                      boxes)
                ro-bindings (emit-readout-bindings layout box-readout-syms)
                outer-bindings
                (vec
                 (for [[oo-idx oo] (map vector (range) outer-outs)
                       :let [ws (dwd/wires-into-outer-out d oo)
                             srcs (mapv #(wire-source-form d % box->b-idx
                                                           box-readout-syms [])
                                        ws)]]
                   [(nth outer-out-syms oo-idx) (sum-form srcs)]))]
            (into ro-bindings outer-bindings)))

        ;; --- Clojure emit ---

        precompute-routing
        (fn []
          (let [wires-by-box-input
                (mapv (fn [b]
                        (let [ips (dwd/box-in-ports d b)]
                          (mapv (fn [ip]
                                  (mapv (fn [w]
                                          (let [[kind ref] (dwd/wire-source d w)]
                                            (case kind
                                              :out-port
                                              (let [src-b (a/subpart d :op-box ref)
                                                    src-b-idx (get box->b-idx src-b)
                                                    src-op-idx (.indexOf
                                                                ^java.util.List
                                                                (dwd/box-out-ports d src-b) ref)]
                                                [:box src-b-idx src-op-idx])
                                              :outer-in
                                              (let [oi-idx (.indexOf
                                                            ^java.util.List
                                                            (vec (dwd/outer-in-ports d)) ref)]
                                                [:outer oi-idx]))))
                                        (dwd/wires-into-in-port d ip)))
                                ips)))
                      boxes)
                outer-out-sources
                (mapv (fn [oo]
                        (mapv (fn [w]
                                (let [[_ ref] (dwd/wire-source d w)
                                      src-b (a/subpart d :op-box ref)
                                      src-b-idx (get box->b-idx src-b)
                                      src-op-idx (.indexOf
                                                  ^java.util.List
                                                  (dwd/box-out-ports d src-b) ref)]
                                  [src-b-idx src-op-idx]))
                              (dwd/wires-into-outer-out d oo)))
                      outer-outs)]
            {:wires-by-box-input wires-by-box-input
             :outer-out-sources outer-out-sources}))

        composite-dynamics-clj
        (fn [layout]
          (let [readout-fns  (mapv (fn [b]
                                     ((:readout-clj (get box->machine b))
                                      (rebuild-box-layout layout b)))
                                   boxes)
                dynamics-fns (mapv (fn [b]
                                     ((:dynamics-clj (get box->machine b))
                                      (rebuild-box-layout layout b)))
                                   boxes)
                {:keys [wires-by-box-input]} (precompute-routing)]
            (fn accumulate [^doubles du ^doubles u ^doubles xs t]
              (let [readouts (mapv (fn [^clojure.lang.IFn rf] (rf u t))
                                   readout-fns)]
                (dotimes [b-idx (count boxes)]
                  (let [m (get box->machine (nth boxes b-idx))
                        n-in (:n-inputs m)
                        inputs (double-array n-in)
                        wires-per-input (nth wires-by-box-input b-idx)]
                    (dotimes [ip-idx n-in]
                      (let [srcs (nth wires-per-input ip-idx)
                            s (reduce (fn [acc tag]
                                        (case (first tag)
                                          :box (+ acc (double
                                                       (nth (nth readouts (nth tag 1))
                                                            (nth tag 2))))
                                          :outer (+ acc (aget xs (nth tag 1)))))
                                      0.0
                                      srcs)]
                        (aset inputs ip-idx s)))
                    ((nth dynamics-fns b-idx) du u inputs t)))))))

        composite-readout-clj
        (fn [layout]
          (let [readout-fns (mapv (fn [b]
                                    ((:readout-clj (get box->machine b))
                                     (rebuild-box-layout layout b)))
                                  boxes)
                {:keys [outer-out-sources]} (precompute-routing)]
            (fn readout [^doubles u t]
              (let [readouts (mapv (fn [^clojure.lang.IFn rf] (rf u t))
                                   readout-fns)]
                (mapv (fn [srcs]
                        (reduce (fn [acc [bi oi]]
                                  (+ acc (double (nth (nth readouts bi) oi))))
                                0.0
                                srcs))
                      outer-out-sources)))))]
    (->Machine n-inputs n-outputs composite-layout
               composite-dynamics-emit composite-readout-emit
               composite-dynamics-clj  composite-readout-clj)))

;; ============================================================================
;; Open-machine runtime — simulate a machine driven by input signals
;; ============================================================================
;;
;; A closed machine (n-inputs = 0) implements RasterCompilable and runs
;; through `compile-rhs` / `compile-clojure-rhs` directly. An OPEN machine
;; (n-inputs > 0) — e.g. a plant driven by a control input, or a composite
;; whose outer-inputs are setpoint signals — needs its inputs supplied.
;; These mirror AlgebraicDynamics' `eval_dynamics` / `readout` / `ODEProblem`.

(defn- input-array
  "Coerce an `xs` of constants and/or (fn [t]) signals into a double-array
   of length n-inputs, sampled at time `t`."
  [m xs t]
  (when (not= (:n-inputs m) (count xs))
    (throw (ex-info "wrong number of inputs"
                    {:expected (:n-inputs m) :got (count xs)})))
  (double-array (map (fn [x] (double (if (fn? x) (x t) x))) xs)))

(defn eval-dynamics
  "Evaluate the machine's vector field at state `u`, inputs `xs`, time `t`.
   Returns the derivative `u̇` as a vector. `xs` is a seq of length
   `n-inputs` of constants and/or `(fn [t])` signals (sampled at `t`)."
  [m u xs t]
  (let [layout (:layout m)
        n      (:size layout)
        acc    ((:dynamics-clj m) layout)
        du     (double-array n)]
    (acc du (double-array u) (input-array m xs t) t)
    (vec du)))

(defn readout
  "The machine's output vector `y` at state `u`, time `t` (state-only)."
  [m u t]
  (((:readout-clj m) (:layout m)) (double-array u) t))

(defn signal-rhs
  "Close a driving signal `xs` over machine `m`, returning a plain
   `(fn [du u t])` derivative suitable for `katzen.petri/integrate-rk4`
   (or any raster-free integrator). `xs` is a seq of length `n-inputs`
   of constants and/or `(fn [t])` signals. Zeros `du` then accumulates,
   matching the integrator contract."
  [m xs]
  (let [layout (:layout m)
        n      (:size layout)
        acc    ((:dynamics-clj m) layout)]
    (fn rhs [^doubles du ^doubles u t]
      (dotimes [i n] (aset du i 0.0))
      (acc du u (input-array m xs t) t))))
