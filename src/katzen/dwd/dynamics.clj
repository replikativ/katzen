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
