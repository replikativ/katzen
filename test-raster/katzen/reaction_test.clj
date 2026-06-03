(ns katzen.reaction-test
  "Tests for reaction networks. Five concerns:

   1. SchReactionNetwork constructors behave like the other ACSet schemas.
   2. The :mass-action rate law produces ODEs identical to a Petri net
      with the same stoichiometry — the cross-check that the new
      framework subsumes Petri.
   3. :michaelis-menten conserves substrate + product and approaches the
      Vmax saturation correctly.
   4. :hill activation reaches its analytical steady state.
   5. :expr lets a user splice in a symbolic rate, validated against
      its analytical steady state."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.compile.core :as cc]
            [katzen.petri :as p]
            [katzen.reaction :as rxn]
            [raster.ode :as ode]))

;; ============================================================================
;; Constructors
;; ============================================================================

(deftest test-reaction-network-empty
  (let [n (rxn/reaction-network)]
    (is (a/acset? n))
    (is (= rxn/SchReactionNetwork (a/schema n)))
    (is (zero? (count (rxn/species n))))
    (is (zero? (count (rxn/reactions n))))))

(deftest test-stoichiometry-tracking
  (testing "Substrate and product multiplicities mirror Petri-style arc-count"
    (let [n (rxn/reaction-network)
          [n s] (rxn/add-species n)
          [n i] (rxn/add-species n)
          [n r] (rxn/add-reaction n)
          [n _] (rxn/add-substrate n s r)
          [n _] (rxn/add-substrate n i r)
          [n _] (rxn/add-product  n i r)
          [n _] (rxn/add-product  n i r)]
      (is (= {r {s 1 i 1}} (rxn/substrate-multiplicity n)))
      (is (= {r {i 2}}     (rxn/product-multiplicity  n))))))

;; ============================================================================
;; :mass-action subsumes Petri
;; ============================================================================

(defn- sir-petri-rhs
  "Direct Petri-net SIR, for cross-checking."
  [beta-N gamma]
  (let [n (p/petri)
        [n s] (p/add-species n)
        [n i] (p/add-species n)
        [n _r] (p/add-species n)
        [n inf] (p/add-transition n)
        [n rec] (p/add-transition n)
        [n _] (p/add-input  n s inf)
        [n _] (p/add-input  n i inf)
        [n _] (p/add-output n i inf)
        [n _] (p/add-output n i inf)
        [n _] (p/add-input  n i rec)
        [n _] (p/add-output n 3 rec)]
    (cc/compile-rhs (p/petri-dynamics n {inf beta-N rec gamma}))))

(defn- sir-rxn-rhs
  "SIR as a reaction network with :mass-action laws."
  [beta-N gamma]
  (let [n (rxn/reaction-network)
        [n s]  (rxn/add-species n)
        [n i]  (rxn/add-species n)
        [n _r] (rxn/add-species n)
        [n r1] (rxn/add-reaction n)
        [n r2] (rxn/add-reaction n)
        [n _]  (rxn/add-substrate n s r1)
        [n _]  (rxn/add-substrate n i r1)
        [n _]  (rxn/add-product  n i r1)
        [n _]  (rxn/add-product  n i r1)
        [n _]  (rxn/add-substrate n i r2)
        [n _]  (rxn/add-product  n 3 r2)]
    (cc/compile-rhs (rxn/reaction-dynamics n {r1 {:type :mass-action :k beta-N}
                                              r2 {:type :mass-action :k gamma}}))))

(deftest test-mass-action-matches-petri-sir
  (testing "ReactionDynamics with :mass-action laws integrates SIR to the
            same trajectory as PetriDynamics"
    (let [beta-N (/ 0.3 1000.0) gamma 0.1
          u0  (double-array [999.0 1.0 0.0])
          rhs-p (sir-petri-rhs beta-N gamma)
          rhs-r (sir-rxn-rhs   beta-N gamma)
          sol-p (ode/solve (ode/tsit5) (ode/ode-problem rhs-p u0 0.0 100.0) 1.0)
          sol-r (ode/solve (ode/tsit5) (ode/ode-problem rhs-r (aclone u0) 0.0 100.0) 1.0)
          fp (vec (last (:us sol-p)))
          fr (vec (last (:us sol-r)))]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth fp k) (nth fr k)))
                     (max 1e-6 (Math/abs (nth fp k))))]
          (is (< rel 1e-10)
              (str "slot " k ": petri=" (nth fp k) " rxn=" (nth fr k))))))))

;; ============================================================================
;; :michaelis-menten
;; ============================================================================

(deftest test-mm-conserves-substrate-plus-product
  (testing "Single S→P reaction with MM kinetics conserves total mass"
    (let [n (rxn/reaction-network)
          [n s] (rxn/add-species n)
          [n p] (rxn/add-species n)
          [n r] (rxn/add-reaction n)
          [n _] (rxn/add-substrate n s r)
          [n _] (rxn/add-product   n p r)
          rd (rxn/reaction-dynamics n {r {:type :michaelis-menten
                                          :Vmax 2.0 :Km 5.0 :substrate s}})
          rhs (cc/compile-rhs rd)
          u0  (double-array [10.0 0.0])
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 10.0) 0.1)]
      (doseq [u (:us sol)]
        (is (< (Math/abs (- (+ (aget u 0) (aget u 1)) 10.0)) 1e-6))))))

(deftest test-mm-zero-substrate-zero-rate
  (testing "When the substrate is zero the MM rate is zero"
    (let [n (rxn/reaction-network)
          [n s] (rxn/add-species n)
          [n p] (rxn/add-species n)
          [n r] (rxn/add-reaction n)
          [n _] (rxn/add-substrate n s r)
          [n _] (rxn/add-product   n p r)
          rd (rxn/reaction-dynamics n {r {:type :michaelis-menten
                                          :Vmax 5.0 :Km 1.0 :substrate s}})
          rhs (cc/compile-rhs rd)
          du (double-array 2)]
      (.invk rhs du (double-array [0.0 7.0]) 0.0)
      (is (= 0.0 (aget du 0)))
      (is (= 0.0 (aget du 1))))))

;; ============================================================================
;; :hill
;; ============================================================================

(deftest test-hill-reaches-analytical-steady-state
  (testing "Hill production with degradation: X_ss = Vmax·A^n / ((K^n + A^n) · k_deg)"
    (let [Vmax 1.0 Km 1.0 n-hill 4 k-deg 0.5 A 2.0
          ;; Analytical: Vmax · A^n / ((K^n + A^n) · k_deg)
          ;; = 1 · 16 / (17 · 0.5) = 32/17
          expected (/ (* Vmax (Math/pow A n-hill))
                      (* (+ (Math/pow Km n-hill) (Math/pow A n-hill)) k-deg))
          n (rxn/reaction-network)
          [n a] (rxn/add-species n)
          [n x] (rxn/add-species n)
          [n rp] (rxn/add-reaction n)
          [n rd] (rxn/add-reaction n)
          [n _]  (rxn/add-product   n x rp)
          [n _]  (rxn/add-substrate n x rd)
          rate-laws {rp {:type :hill :Vmax Vmax :Km Km :n n-hill :substrate a}
                     rd {:type :mass-action :k k-deg}}
          rhs (cc/compile-rhs (rxn/reaction-dynamics n rate-laws))
          u0 (double-array [A 0.0])
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 40.0) 0.5)
          X-final (aget ^doubles (last (:us sol)) 1)]
      (is (< (Math/abs (- X-final expected)) 0.01)
          (str "expected " expected ", got " X-final)))))

;; ============================================================================
;; :expr
;; ============================================================================

(deftest test-expr-feedback-inhibition
  (testing "Custom rate law: dY/dt = 1/(1+X^2) - k·Y reaches the expected
            steady state Y = 1/((1+X^2)·k)"
    (let [k 0.1 X-fixed 3.0
          expected (/ 1.0 (* (+ 1.0 (* X-fixed X-fixed)) k))   ;; = 1.0
          n (rxn/reaction-network)
          [n x] (rxn/add-species n)
          [n y] (rxn/add-species n)
          [n rp] (rxn/add-reaction n)
          [n rd] (rxn/add-reaction n)
          [n _]  (rxn/add-product   n y rp)
          [n _]  (rxn/add-substrate n y rd)
          rate-laws {rp {:type :expr
                         :form '(/ 1.0 (+ 1.0 (Math/pow X 2)))
                         :bindings {'X x}}
                     rd {:type :mass-action :k k}}
          rhs (cc/compile-rhs (rxn/reaction-dynamics n rate-laws))
          u0  (double-array [X-fixed 0.0])
          sol (ode/solve (ode/tsit5) (ode/ode-problem rhs u0 0.0 200.0) 1.0)
          Y-final (aget ^doubles (last (:us sol)) 1)]
      (is (< (Math/abs (- Y-final expected)) 0.005)
          (str "expected " expected ", got " Y-final)))))

;; ============================================================================
;; Clojure fallback paths consistent
;; ============================================================================

(deftest test-clojure-and-raster-paths-agree
  (testing "compile-clojure-rhs and compile-rhs produce the same SIR trajectory
            to within solver tolerance"
    (let [rhs-r (sir-rxn-rhs (/ 0.3 1000.0) 0.1)
          rd    (let [n (rxn/reaction-network)
                      [n s] (rxn/add-species n)
                      [n i] (rxn/add-species n)
                      [n _r] (rxn/add-species n)
                      [n r1] (rxn/add-reaction n)
                      [n r2] (rxn/add-reaction n)
                      [n _]  (rxn/add-substrate n s r1)
                      [n _]  (rxn/add-substrate n i r1)
                      [n _]  (rxn/add-product  n i r1)
                      [n _]  (rxn/add-product  n i r1)
                      [n _]  (rxn/add-substrate n i r2)
                      [n _]  (rxn/add-product  n 3 r2)]
                  (rxn/reaction-dynamics n {r1 {:type :mass-action :k (/ 0.3 1000.0)}
                                            r2 {:type :mass-action :k 0.1}}))
          rhs-c (cc/compile-clojure-rhs rd)
          u0    (double-array [999.0 1.0 0.0])
          sol-r (ode/solve (ode/tsit5) (ode/ode-problem rhs-r u0 0.0 100.0) 1.0)
          sol-c (p/integrate-rk4 rhs-c [999.0 1.0 0.0] 0.0 100.0 0.05)
          fr (vec (last (:us sol-r)))
          fc (vec (last (:us sol-c)))]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth fr k) (nth fc k)))
                     (max 1e-6 (Math/abs (nth fr k))))]
          (is (< rel 0.01) (str "slot " k " diff=" rel)))))))
