(ns katzen.cpg-test
  "Tests for circular port graphs and their dynamics composition.

   The key validation is a coupled mutual-feedback scenario: two
   identical 'mirror' boxes, each with 2 ports. Two edges connect
   B1.port0 ↔ B2.port0, making each box receive the other's state.
   Dynamics: dx1/dt = -x1 + x2, dx2/dt = -x2 + x1. The composite
   conserves x1+x2 and decays x1-x2 at rate 2, so this gives us
   two independent analytical checks."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.compile.core :as cc]
            [katzen.cpg :as cpg]
            [katzen.dwd.dynamics :as mach]
            [raster.ode :as ode]))

;; ============================================================================
;; Constructors and the SchCPG round-trip
;; ============================================================================

(deftest test-empty-cpg
  (let [g (cpg/cpg)]
    (is (a/acset? g))
    (is (= cpg/SchCPG (a/schema g)))
    (is (zero? (count (cpg/boxes g))))))

(deftest test-add-cpg-box-with-ports
  (let [g (cpg/cpg)
        [g B ports] (cpg/add-cpg-box-with-ports g 3)]
    (is (= 1 (count (cpg/boxes g))))
    (is (= 3 (count ports)))
    (is (every? #(= B (a/subpart g :port-box %)) ports))
    (is (= 3 (cpg/nports g B)))))

(deftest test-add-edge
  (let [g (cpg/cpg)
        [g _ [p1]] (cpg/add-cpg-box-with-ports g 1)
        [g _ [p2]] (cpg/add-cpg-box-with-ports g 1)
        [g e] (cpg/add-edge g p1 p2)]
    (is (= p1 (cpg/edge-src g e)))
    (is (= p2 (cpg/edge-tgt g e)))))

;; ============================================================================
;; cpg->dwd translation
;; ============================================================================

(deftest test-cpg-to-dwd-translation-shape
  (testing "Each CPG port becomes one DWD InPort and one DWD OutPort on
            the corresponding DWD box; each edge becomes one wire"
    (let [g (cpg/cpg)
          [g _ _] (cpg/add-cpg-box-with-ports g 2)
          [g _ _] (cpg/add-cpg-box-with-ports g 2)
          [g _]   (cpg/add-edge g 1 3)
          [g _]   (cpg/add-edge g 3 1)
          translation (cpg/cpg->dwd g)
          d (:dwd translation)]
      (is (= 2 (count (a/parts d :Box))))
      (is (= 4 (count (a/parts d :InPort))))
      (is (= 4 (count (a/parts d :OutPort))))
      (is (= 2 (count (a/parts d :Wire)))))))

;; ============================================================================
;; oapply-cpg integration
;; ============================================================================

(def coupled-box
  "Each box has 2 ports. Dynamics: dx/dt = -x + sum(inputs).
   Both ports readout the state."
  (mach/machine
   {:state-labels [:x]
    :n-inputs 2  :n-outputs 2
    :dynamics-emit
    (fn [layout input-syms]
      [(let [s (cc/slot layout :x)]
         `(raster.arrays/aset ~'du ~s
            (raster.numeric/+ (raster.arrays/aget ~'du ~s)
              (raster.numeric/+ (raster.numeric/* -1.0 (raster.arrays/aget ~'u ~s))
                                (raster.numeric/+ ~(first input-syms)
                                                  ~(second input-syms))))))])
    :readout-emit
    (fn [layout output-syms]
      (let [s (cc/slot layout :x)]
        [[(first output-syms)  `(raster.arrays/aget ~'u ~s)]
         [(second output-syms) `(raster.arrays/aget ~'u ~s)]]))
    :dynamics-clj
    (fn [layout]
      (let [s (cc/slot layout :x)]
        (fn [^doubles du ^doubles u ^doubles xs _t]
          (aset du s (+ (aget du s)
                        (+ (* -1.0 (aget u s))
                           (+ (aget xs 0) (aget xs 1))))))))
    :readout-clj
    (fn [layout]
      (let [s (cc/slot layout :x)]
        (fn [^doubles u _t]
          (let [x (aget u s)] [x x]))))}))

(defn- coupled-cpg
  "Build the mutual-feedback CPG and the composite Machine."
  []
  (let [g (cpg/cpg)
        [g B1 [B1p0 _]] (cpg/add-cpg-box-with-ports g 2)
        [g B2 [B2p0 _]] (cpg/add-cpg-box-with-ports g 2)
        [g _] (cpg/add-edge g B1p0 B2p0)
        [g _] (cpg/add-edge g B2p0 B1p0)]
    (cpg/oapply-cpg g {B1 coupled-box B2 coupled-box})))

(deftest test-coupled-cpg-conserves-sum
  (testing "x1 + x2 stays at 1.0 throughout integration"
    (let [composite (coupled-cpg)
          rhs (cc/compile-rhs composite)
          u0  (double-array [1.0 0.0])
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 2.0) 0.1)]
      (doseq [u (:us sol)]
        (is (< (Math/abs (- (+ (aget u 0) (aget u 1)) 1.0)) 1e-6))))))

(deftest test-coupled-cpg-difference-decays-at-rate-2
  (testing "x1 - x2 decays as e^(-2t) — the dominant antisymmetric mode"
    (let [composite (coupled-cpg)
          rhs (cc/compile-rhs composite)
          u0  (double-array [1.0 0.0])
          T 1.5
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 T) 0.05)
          final (last (:us sol))
          diff (- (aget final 0) (aget final 1))
          expected (Math/exp (* -2.0 T))]
      (is (< (Math/abs (- diff expected)) 1e-5)
          (str "x1-x2=" diff " expected " expected)))))

(deftest test-cpg-rejects-mismatched-machine
  (testing "Fails fast if a box's machine doesn't match its port count"
    (let [;; 1-port box with a 2-port machine
          g (cpg/cpg)
          [g B _] (cpg/add-cpg-box-with-ports g 1)]
      (is (thrown-with-msg? Exception #"n-inputs = n-outputs"
                            (cpg/oapply-cpg g {B coupled-box}))))))
