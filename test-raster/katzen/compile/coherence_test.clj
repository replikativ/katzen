(ns katzen.compile.coherence-test
  "Operadic coherence checks for `oapply` over UWDs and DWDs. Validates
   the categorical laws by comparing trajectories under different
   compositions of the same logical system:

   - **Identity composition**: composing a value with an identity-shaped
     UWD/DWD gives a trajectory equal to the original.
   - **Hierarchical vs flat composition**: when a Machine M can be expressed
     either as a single 'flat' composite or as a 'nested' composite
     (one DWD whose box is itself a DWD), the two paths must produce
     the same trajectory.

   These are operational rather than structural — we compare integrated
   trajectories, not emitted source. That's the right level given the
   composite-emit code uses gensyms internally (source comparison
   would be over-strict).

   Lives under test-raster because the integration step needs raster."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.dwd :as dwd]
            [katzen.dwd.dynamics :as mach]
            [raster.ode :as ode]))

;; ============================================================================
;; Shared builders — single-state decay machine variants
;; ============================================================================

(def decay-no-input
  (mach/machine
   {:state-labels [:x]  :n-inputs 0 :n-outputs 1
    :dynamics-emit (fn [layout _]
                     [(let [s (cc/slot layout :x)]
                        `(raster.arrays/aset ~'du ~s
                           (raster.numeric/+ (raster.arrays/aget ~'du ~s)
                                             (raster.numeric/* -1.0
                                                               (raster.arrays/aget ~'u ~s)))))])
    :readout-emit (fn [layout outs]
                    [[(first outs) `(raster.arrays/aget ~'u ~(cc/slot layout :x))]])
    :dynamics-clj (fn [layout]
                    (let [s (cc/slot layout :x)]
                      (fn [^doubles du ^doubles u _xs _t]
                        (aset du s (+ (aget du s) (* -1.0 (aget u s)))))))
    :readout-clj  (fn [layout]
                    (let [s (cc/slot layout :x)]
                      (fn [^doubles u _t] [(aget u s)])))}))

(def decay-driven
  (mach/machine
   {:state-labels [:x]  :n-inputs 1 :n-outputs 1
    :dynamics-emit (fn [layout in-syms]
                     [(let [s  (cc/slot layout :x)
                            in (first in-syms)]
                        `(raster.arrays/aset ~'du ~s
                           (raster.numeric/+ (raster.arrays/aget ~'du ~s)
                                             (raster.numeric/+ (raster.numeric/* -1.0
                                                                                 (raster.arrays/aget ~'u ~s))
                                                               ~in))))])
    :readout-emit (fn [layout outs]
                    [[(first outs) `(raster.arrays/aget ~'u ~(cc/slot layout :x))]])
    :dynamics-clj (fn [layout]
                    (let [s (cc/slot layout :x)]
                      (fn [^doubles du ^doubles u ^doubles xs _t]
                        (aset du s (+ (aget du s)
                                      (+ (* -1.0 (aget u s)) (aget xs 0)))))))
    :readout-clj  (fn [layout]
                    (let [s (cc/slot layout :x)]
                      (fn [^doubles u _t] [(aget u s)])))}))

;; ============================================================================
;; Composite builders — two paths to the same 3-stage cascade
;; ============================================================================

(defn- flat-cascade
  "Build the 3-stage cascade as ONE DWD with three boxes."
  []
  (let [d (dwd/dwd)
        [d Bs _   [op-s]]    (dwd/add-box-with-ports d 0 1)
        [d B1 [ip-1] [op-1]] (dwd/add-box-with-ports d 1 1)
        [d B2 [ip-2] [_]]    (dwd/add-box-with-ports d 1 1)
        [d _] (dwd/add-box-wire d op-s ip-1)
        [d _] (dwd/add-box-wire d op-1 ip-2)]
    (mach/oapply-dwd d {Bs decay-no-input
                        B1 decay-driven
                        B2 decay-driven})))

(defn- nested-cascade
  "Build the 3-stage cascade as a NESTED DWD: an inner Machine wrapping
   a single decay-driven, used twice as sub-boxes of an outer DWD."
  []
  (let [inner-d (let [d (dwd/dwd)
                      [d B [ip] [op]] (dwd/add-box-with-ports d 1 1)
                      [d oin]  (dwd/add-outer-in-port d)
                      [d oout] (dwd/add-outer-out-port d)
                      [d _] (dwd/add-input-wire  d oin ip)
                      [d _] (dwd/add-output-wire d op  oout)]
                  {:d d :B B})
        inner (mach/oapply-dwd (:d inner-d) {(:B inner-d) decay-driven})
        outer-d (let [d (dwd/dwd)
                      [d Bs _   [op-s]]    (dwd/add-box-with-ports d 0 1)
                      [d B1 [ip-1] [op-1]] (dwd/add-box-with-ports d 1 1)
                      [d B2 [ip-2] [_]]    (dwd/add-box-with-ports d 1 1)
                      [d _] (dwd/add-box-wire d op-s ip-1)
                      [d _] (dwd/add-box-wire d op-1 ip-2)]
                  {:d d :Bs Bs :B1 B1 :B2 B2})]
    (mach/oapply-dwd (:d outer-d)
                     {(:Bs outer-d) decay-no-input
                      (:B1 outer-d) inner
                      (:B2 outer-d) inner})))

(defn- integrate-final
  "Integrate `composite` from u0 to t=T and return the final state."
  [composite u0 T]
  (let [rhs (cc/compile-rhs composite)
        sol (ode/solve (ode/tsit5) (ode/ode-problem rhs (double-array u0) 0.0 T) 0.05)]
    (vec (last (:us sol)))))

;; ============================================================================
;; Coherence: nested == flat
;; ============================================================================

(deftest test-flat-and-nested-cascade-agree
  (testing "The 3-stage cascade composed flat and nested produces the same
            trajectory — operadic coherence of oapply-dwd"
    (let [u0 [1.0 0.0 0.0]
          T 2.0
          flat   (integrate-final (flat-cascade)   u0 T)
          nested (integrate-final (nested-cascade) u0 T)]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth flat k) (nth nested k)))
                     (max 1e-9 (Math/abs (nth flat k))))]
          (is (< rel 1e-8)
              (str "slot " k ": flat=" (nth flat k) " nested=" (nth nested k))))))))

;; ============================================================================
;; Identity composition (degenerate, just confirms a 1-box DWD ≡ the box)
;; ============================================================================

(deftest test-singleton-dwd-equals-the-underlying-machine
  (testing "A DWD with one box that has no wires reproduces the box's
            trajectory exactly"
    (let [d (dwd/dwd)
          [d B _ _] (dwd/add-box-with-ports d 0 0)
          composite (mach/oapply-dwd d {B decay-no-input})
          u0 [1.0]
          T 2.0
          composite-final (integrate-final composite u0 T)
          direct-final    (let [rhs (cc/compile-rhs decay-no-input)
                                sol (ode/solve (ode/tsit5)
                                               (ode/ode-problem rhs (double-array u0) 0.0 T)
                                               0.05)]
                            (vec (last (:us sol))))]
      (is (< (Math/abs (- (first composite-final) (first direct-final))) 1e-12)))))
