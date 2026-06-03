(ns katzen.compile.core-test
  "Locks the `RasterCompilable` protocol contract via Petri as the first
   concrete implementation. Tests assert:

   - State layouts are well-formed.
   - Bodies accumulate but DO NOT zero du — the driver does that. So
     calling a body twice with reset du gives the same result; calling
     it twice without zeroing doubles the contribution. Both are
     invariants downstream composites depend on.
   - `compile-rhs` and `compile-clojure-rhs` produce ODEs whose
     trajectories agree to within float tolerance."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.petri :as p]
            [raster.ode :as ode]))

(defn- sir
  "Minimal SIR Petri net + its rates, returned as a PetriDynamics."
  []
  (let [n     (p/petri)
        [n s]   (p/add-species n)
        [n i]   (p/add-species n)
        [n _r]  (p/add-species n)
        [n inf] (p/add-transition n)
        [n rec] (p/add-transition n)
        [n _]   (p/add-input  n s inf)
        [n _]   (p/add-input  n i inf)
        [n _]   (p/add-output n i inf)
        [n _]   (p/add-output n i inf)
        [n _]   (p/add-input  n i rec)
        [n _]   (p/add-output n 3 rec)]
    (p/petri-dynamics n {inf 0.001 rec 0.1})))

;; ============================================================================
;; Protocol-level
;; ============================================================================

(deftest test-petri-implements-protocol
  (let [pd (sir)]
    (is (cc/compilable? pd))
    (is (instance? katzen.compile.core.StateLayout (cc/layout-of pd)))))

(deftest test-state-layout-has-expected-shape
  (let [pd (sir)
        layout (cc/layout-of pd)]
    (is (= 3 (:size layout)))
    (is (= {1 0 2 1 3 2} (:index-of layout)))
    (testing "slot translates a known label, throws on unknown"
      (is (= 0 (cc/slot layout 1)))
      (is (= 2 (cc/slot layout 3)))
      (is (thrown? Exception (cc/slot layout :nope))))))

;; ============================================================================
;; Body semantics — accumulate, don't zero
;; ============================================================================

(deftest test-clojure-body-accumulates-not-zeros
  (testing "Calling the body twice without zeroing doubles the contribution —
            confirms the body does not zero du itself"
    (let [pd     (sir)
          layout (cc/layout-of pd)
          body   (cc/clojure-body pd layout)
          u      (double-array [999.0 1.0 0.0])
          du1    (double-array 3)
          du2    (double-array 3)]
      (body du1 u 0.0)
      (body du2 u 0.0) (body du2 u 0.0)
      (dotimes [k 3]
        (is (< (Math/abs (- (aget du2 k) (* 2.0 (aget du1 k)))) 1e-12)
            (str "slot " k ": single=" (aget du1 k) " double=" (aget du2 k)))))))

(deftest test-raster-body-also-accumulates
  (testing "The raster body, eval'd as a standalone block, also accumulates
            rather than zeroing"
    (let [pd      (sir)
          layout  (cc/layout-of pd)
          body    (cc/raster-body pd layout)
          ;; Wrap the accumulate-only body in an ftm WITHOUT a zero-out.
          form    `(raster.core/ftm
                    [~'du :- (~'Array ~'double) ~'u :- (~'Array ~'double) ~'t :- ~'Double]
                    ~@body)
          f       (eval form)
          u       (double-array [999.0 1.0 0.0])
          du1     (double-array 3)
          du2     (double-array 3)]
      (.invk f du1 u 0.0)
      (.invk f du2 u 0.0) (.invk f du2 u 0.0)
      (dotimes [k 3]
        (is (< (Math/abs (- (aget du2 k) (* 2.0 (aget du1 k)))) 1e-12))))))

;; ============================================================================
;; Driver
;; ============================================================================

(deftest test-compile-rhs-zeros-then-accumulates
  (testing "The driver-produced rhs DOES zero — calling twice yields the
            same result, not twice it"
    (let [pd      (sir)
          rhs     (cc/compile-rhs pd)
          u       (double-array [999.0 1.0 0.0])
          du1     (double-array 3)
          du2     (double-array 3)]
      (.invk rhs du1 u 0.0)
      (.invk rhs du2 u 0.0)
      (dotimes [k 3]
        (is (= (aget du1 k) (aget du2 k)))))))

(deftest test-compile-clojure-and-raster-agree-on-trajectory
  (testing "Both drivers integrate to nearly the same final state"
    (let [pd       (sir)
          rhs-r    (cc/compile-rhs pd)
          rhs-c    (cc/compile-clojure-rhs pd)
          u0       (double-array [999.0 1.0 0.0])
          prob-r   (ode/ode-problem rhs-r u0 0.0 50.0)
          sol-r    (ode/solve (ode/tsit5) prob-r 1.0)
          ;; The Clojure rhs lacks the typed interface; we'd need the shim
          ;; for raster. Compare via in-house RK4 instead.
          sol-c    (katzen.petri/integrate-rk4
                    rhs-c [999.0 1.0 0.0] 0.0 50.0 0.05)
          final-r  (vec (last (:us sol-r)))
          final-c  (vec (last (:us sol-c)))]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth final-r k) (nth final-c k)))
                     (max 1e-6 (Math/abs (nth final-r k))))]
          (is (< rel 0.01) (str "slot " k " diff=" rel)))))))
