(ns katzen.dwd.dynamics-test
  "Tests for the DWD Machines layer.

   The key validation is the cascade decay scenario:
     - Machine 1: dx1/dt = -x1, output = x1
     - Machine 2: dx2/dt = -x2 + input, output = x2
     - Wire: M1 → M2
   The composite system dx1/dt = -x1, dx2/dt = -x2 + x1 has a closed-form
   solution x1(t) = x1(0)·e^(-t), x2(t) = (x2(0) + x1(0)·t)·e^(-t)
   that we hit within Tsit5 tolerance.

   We also check shape: composite state layout is the disjoint union
   (not coequalized as in CRS/UWD), and the composite implements
   `RasterCompilable`."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.compile.core :as cc]
            [katzen.dwd :as dwd]
            [katzen.dwd.dynamics :as mach]
            [raster.ode :as ode]))

;; ============================================================================
;; Decay machine builders
;; ============================================================================

(def decay-no-input
  "Plain decay: dx/dt = -x. One output equal to x. No inputs."
  (mach/machine
   {:state-labels [:x]
    :n-inputs 0
    :n-outputs 1
    :dynamics-emit
    (fn [layout _]
      [(let [s (cc/slot layout :x)]
         `(raster.arrays/aset ~'du ~s
            (raster.numeric/+ (raster.arrays/aget ~'du ~s)
                              (raster.numeric/* -1.0 (raster.arrays/aget ~'u ~s)))))])
    :readout-emit
    (fn [layout output-syms]
      [[(first output-syms) `(raster.arrays/aget ~'u ~(cc/slot layout :x))]])
    :dynamics-clj
    (fn [layout]
      (let [s (cc/slot layout :x)]
        (fn [^doubles du ^doubles u _ _t]
          (aset du s (+ (aget du s) (* -1.0 (aget u s)))))))
    :readout-clj
    (fn [layout]
      (let [s (cc/slot layout :x)]
        (fn [^doubles u _t] [(aget u s)])))}))

(def decay-driven
  "Driven decay: dx/dt = -x + input. One input, one output (= x)."
  (mach/machine
   {:state-labels [:x]
    :n-inputs 1
    :n-outputs 1
    :dynamics-emit
    (fn [layout input-syms]
      [(let [s (cc/slot layout :x)
             in (first input-syms)]
         `(raster.arrays/aset ~'du ~s
            (raster.numeric/+ (raster.arrays/aget ~'du ~s)
                              (raster.numeric/+ (raster.numeric/* -1.0 (raster.arrays/aget ~'u ~s))
                                                ~in))))])
    :readout-emit
    (fn [layout output-syms]
      [[(first output-syms) `(raster.arrays/aget ~'u ~(cc/slot layout :x))]])
    :dynamics-clj
    (fn [layout]
      (let [s (cc/slot layout :x)]
        (fn [^doubles du ^doubles u ^doubles xs _t]
          (aset du s (+ (aget du s) (+ (* -1.0 (aget u s)) (aget xs 0)))))))
    :readout-clj
    (fn [layout]
      (let [s (cc/slot layout :x)]
        (fn [^doubles u _t] [(aget u s)])))}))

;; ============================================================================
;; DWDs
;; ============================================================================

(deftest test-dwd-construction
  (testing "Basic boxes + ports + wire"
    (let [d (dwd/dwd)
          [d B1 ips1 ops1] (dwd/add-box-with-ports d 1 1)
          [d B2 ips2 ops2] (dwd/add-box-with-ports d 1 1)
          [d w] (dwd/add-box-wire d (first ops1) (first ips2))]
      (is (= 2 (count (dwd/boxes d))))
      (is (= 2 (count (dwd/in-ports d))))
      (is (= 2 (count (dwd/out-ports d))))
      (is (= 1 (count (dwd/wires d))))
      (testing "Wire source/target queries"
        (is (= [:out-port (first ops1)] (dwd/wire-source d w)))
        (is (= [:in-port  (first ips2)] (dwd/wire-target d w)))))))

(deftest test-wires-into-in-port-and-outer-out
  (testing "incident lookups for routing"
    (let [d (dwd/dwd)
          [d B1 _ops1in [op1]] (dwd/add-box-with-ports d 0 1)
          [d B2 [ip2 ip2'] _ops2] (dwd/add-box-with-ports d 2 0)
          [d w1] (dwd/add-box-wire d op1 ip2)
          [d w2] (dwd/add-box-wire d op1 ip2')]
      (is (= [w1] (vec (dwd/wires-into-in-port d ip2))))
      (is (= [w2] (vec (dwd/wires-into-in-port d ip2')))))))

;; ============================================================================
;; oapply-dwd — the cascade
;; ============================================================================

(defn- cascade-composite
  "Compose decay-no-input → decay-driven."
  []
  (let [d (dwd/dwd)
        [d B1 _ [op1]] (dwd/add-box-with-ports d 0 1)
        [d B2 [ip2] _] (dwd/add-box-with-ports d 1 1)
        [d _] (dwd/add-box-wire d op1 ip2)]
    {:dwd d :B1 B1 :B2 B2
     :composite (mach/oapply-dwd d {B1 decay-no-input B2 decay-driven})}))

(deftest test-composite-state-layout-is-disjoint-union
  (let [{:keys [composite B1 B2]} (cascade-composite)]
    (let [layout (cc/layout-of composite)]
      (is (= 2 (:size layout)) "disjoint union of two single-state boxes")
      (is (= 0 (cc/slot layout [B1 :x])))
      (is (= 1 (cc/slot layout [B2 :x]))))))

(deftest test-composite-implements-raster-compilable
  (let [{:keys [composite]} (cascade-composite)]
    (is (cc/compilable? composite))))

(deftest test-cascade-matches-analytical-solution
  (testing "Composite cascade integrates to the analytical solution"
    (let [{:keys [composite]} (cascade-composite)
          rhs  (cc/compile-rhs composite)
          x1-0 1.0  x2-0 0.0
          T    2.0
          u0   (double-array [x1-0 x2-0])
          sol  (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 T) 0.05)
          final (last (:us sol))
          x1-final-expected (* x1-0 (Math/exp (- T)))                ;; e^(-T)
          x2-final-expected (* (+ x2-0 (* x1-0 T)) (Math/exp (- T)))]
      (is (< (Math/abs (- (aget final 0) x1-final-expected)) 1e-5))
      (is (< (Math/abs (- (aget final 1) x2-final-expected)) 1e-5)))))

(deftest test-cascade-clojure-rhs-also-matches
  (testing "compile-clojure-rhs reproduces the same cascade trajectory"
    (let [{:keys [composite]} (cascade-composite)
          rhs (cc/compile-clojure-rhs composite)
          x1-0 1.0  x2-0 0.0
          T    2.0
          sol  (katzen.petri/integrate-rk4 rhs [x1-0 x2-0] 0.0 T 0.005)
          final (last (:us sol))
          x1-expected (* x1-0 (Math/exp (- T)))
          x2-expected (* (+ x2-0 (* x1-0 T)) (Math/exp (- T)))]
      (is (< (Math/abs (- (aget final 0) x1-expected)) 1e-4))
      (is (< (Math/abs (- (aget final 1) x2-expected)) 1e-4)))))

;; ============================================================================
;; Multiple wires summing into one port
;; ============================================================================

;; ============================================================================
;; Nested DWD composition
;; ============================================================================

(deftest test-composite-machine-has-correct-port-counts
  (testing "When a DWD has outer-input and outer-output ports, the composite
            Machine exposes them as its own n-inputs / n-outputs"
    (let [d (dwd/dwd)
          [d B [ip] [op]] (dwd/add-box-with-ports d 1 1)
          [d oin]  (dwd/add-outer-in-port d)
          [d oout] (dwd/add-outer-out-port d)
          [d _] (dwd/add-input-wire  d oin ip)
          [d _] (dwd/add-output-wire d op  oout)
          composite (mach/oapply-dwd d {B decay-driven})]
      (is (mach/machine? composite))
      (is (= 1 (:n-inputs composite)))
      (is (= 1 (:n-outputs composite))))))

(deftest test-machine-with-inputs-rejects-direct-compile
  (testing "A Machine with n-inputs > 0 throws when compiled standalone"
    (let [d (dwd/dwd)
          [d B [ip] _] (dwd/add-box-with-ports d 1 0)
          [d oin] (dwd/add-outer-in-port d)
          [d _] (dwd/add-input-wire d oin ip)
          composite (mach/oapply-dwd d {B decay-driven})]
      (is (thrown-with-msg? Exception #"n-inputs"
                            (cc/compile-rhs composite))))))

(deftest test-nested-dwd-three-stage-cascade
  (testing "Composing two single-box Machines as sub-Machines of an outer
            DWD yields the analytical 3-stage cascade solution"
    (let [;; Inner DWD: 1 outer-input → decay-driven → 1 outer-output.
          inner-d (let [d (dwd/dwd)
                        [d B [ip] [op]] (dwd/add-box-with-ports d 1 1)
                        [d oin]  (dwd/add-outer-in-port d)
                        [d oout] (dwd/add-outer-out-port d)
                        [d _] (dwd/add-input-wire  d oin ip)
                        [d _] (dwd/add-output-wire d op  oout)]
                    {:d d :B B})
          inner-machine (mach/oapply-dwd (:d inner-d)
                                         {(:B inner-d) decay-driven})
          ;; Outer DWD: source → inner_1 → inner_2 (each is a 1-box Machine).
          outer-d (let [d (dwd/dwd)
                        [d Bs _   [op-s]]  (dwd/add-box-with-ports d 0 1)
                        [d B1 [ip-1] [op-1]] (dwd/add-box-with-ports d 1 1)
                        [d B2 [ip-2] [_]]   (dwd/add-box-with-ports d 1 1)
                        [d _] (dwd/add-box-wire d op-s ip-1)
                        [d _] (dwd/add-box-wire d op-1 ip-2)]
                    {:d d :Bs Bs :B1 B1 :B2 B2})
          composite (mach/oapply-dwd (:d outer-d)
                                     {(:Bs outer-d) decay-no-input
                                      (:B1 outer-d) inner-machine
                                      (:B2 outer-d) inner-machine})
          rhs (cc/compile-rhs composite)
          u0  (double-array [1.0 0.0 0.0])
          T   2.0
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 T) 0.05)
          final (last (:us sol))
          ;; 3-stage cascade: dx/dt=-x, dy/dt=-y+x, dz/dt=-z+y
          ;; (with x0=1, y0=z0=0)
          ;;   x(t) = e^(-t)
          ;;   y(t) = t e^(-t)
          ;;   z(t) = t^2/2 e^(-t)
          x-exp (Math/exp (- T))
          y-exp (* T x-exp)
          z-exp (* 0.5 T T x-exp)]
      (is (< (Math/abs (- (aget final 0) x-exp)) 1e-5))
      (is (< (Math/abs (- (aget final 1) y-exp)) 1e-5))
      (is (< (Math/abs (- (aget final 2) z-exp)) 1e-5)))))

(deftest test-multiple-wires-sum-at-target
  (testing "Two boxes both feeding into one input — the input is the sum"
    (let [d (dwd/dwd)
          [d B1 _ [op1]] (dwd/add-box-with-ports d 0 1)
          [d B2 _ [op2]] (dwd/add-box-with-ports d 0 1)
          [d B3 [ip3] _] (dwd/add-box-with-ports d 1 1)
          [d _] (dwd/add-box-wire d op1 ip3)
          [d _] (dwd/add-box-wire d op2 ip3)
          composite (mach/oapply-dwd d {B1 decay-no-input
                                        B2 decay-no-input
                                        B3 decay-driven})
          rhs (cc/compile-rhs composite)
          ;; x1(0)=1, x2(0)=1, x3(0)=0
          ;; dx1/dt = -x1, dx2/dt = -x2, dx3/dt = -x3 + x1 + x2
          ;; Analytical: x1=x2=e^(-t); x3 = (x3(0) + 2t)·e^(-t)
          u0 (double-array [1.0 1.0 0.0])
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 2.0) 0.05)
          final (last (:us sol))
          expected-x3 (* 2.0 2.0 (Math/exp -2.0))]   ;; (0 + 2·2)·e^(-2)
      (is (< (Math/abs (- (aget final 2) expected-x3)) 1e-5)))))
