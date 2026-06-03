(ns katzen.petri-test
  "Tests for SchPetri, mass-action derivatives, in-house RK4 integration, and
   UWD-based Petri composition (the SIR example as a regression check)."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.petri :as p]
            [katzen.uwd :as uwd]))

;; ============================================================================
;; SchPetri + constructors
;; ============================================================================

(deftest test-empty-petri
  (let [n (p/petri)]
    (is (a/acset? n))
    (is (= p/SchPetri (a/schema n)))
    (is (= 0 (count (p/species n))))
    (is (= 0 (count (p/transitions n))))))

(defn- sir-direct
  "Build the SIR Petri net by direct construction. Returns [net inf-id rec-id]."
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
    [n inf rec]))

(deftest test-sir-multiplicities
  (testing "SIR multiplicities match the textbook structure"
    (let [[n inf rec] (sir-direct)]
      (is (= {inf {1 1 2 1} rec {2 1}} (p/in-multiplicity n)))
      (is (= {inf {2 2}    rec {3 1}} (p/out-multiplicity n))))))

;; ============================================================================
;; Mass-action derivative
;; ============================================================================

(deftest test-derivative-matches-textbook-sir
  (testing "At u=[S, I, R] the derivative equals (-βSI, βSI - γI, γI)"
    (let [[n inf rec] (sir-direct)
          beta 0.001 gamma 0.1
          f    (p/petri->derivative n {inf beta rec gamma})
          u    (double-array [999.0 1.0 0.0])
          du   (double-array 3)]
      (f du u 0.0)
      (is (< (Math/abs (- (aget du 0) (- (* beta 999.0 1.0)))) 1e-12))
      (is (< (Math/abs (- (aget du 1) (- (* beta 999.0 1.0) (* gamma 1.0)))) 1e-12))
      (is (< (Math/abs (- (aget du 2) (* gamma 1.0))) 1e-12)))))

(deftest test-derivative-respects-multiplicity
  (testing "Quadratic input multiplicity m means rate ∝ X^m"
    ;; 2X → Y at rate k: dX/dt = -2 * k * X^2, dY/dt = k * X^2
    (let [n (p/petri)
          [n x] (p/add-species n)
          [n y] (p/add-species n)
          [n t] (p/add-transition n)
          [n _] (p/add-input n x t)
          [n _] (p/add-input n x t)        ; two X arcs → multiplicity 2
          [n _] (p/add-output n y t)
          k 0.5
          f (p/petri->derivative n {t k})
          u (double-array [3.0 0.0])
          du (double-array 2)]
      (f du u 0.0)
      (is (< (Math/abs (- (aget du 0) (- (* 2.0 k 9.0)))) 1e-12))
      (is (< (Math/abs (- (aget du 1) (* k 9.0))) 1e-12)))))

;; ============================================================================
;; RK4 integration
;; ============================================================================

(deftest test-rk4-conserves-sir-total-population
  (testing "Total S+I+R stays equal to N within RK4's accuracy across all
            saved time points"
    (let [[n inf rec] (sir-direct)
          N    1000.0
          f    (p/petri->derivative n {inf (/ 0.3 N) rec 0.1})
          sol  (p/integrate-rk4 f [(- N 1.0) 1.0 0.0] 0.0 100.0 0.1 10)]
      (doseq [u (:us sol)]
        (let [total (+ (aget u 0) (aget u 1) (aget u 2))]
          (is (< (Math/abs (- total N)) 1e-6)
              (str "population leak: total=" total " at saved point")))))))

(deftest test-rk4-reproduces-known-sir-trajectory
  (testing "With (β,γ,N) = (0.3, 0.1, 1000) and one initial infected, the
            peak infected count comes in near 300 and final R near 940 —
            classic textbook SIR numbers"
    (let [[n inf rec] (sir-direct)
          N    1000.0
          f    (p/petri->derivative n {inf (/ 0.3 N) rec 0.1})
          sol  (p/integrate-rk4 f [(- N 1.0) 1.0 0.0] 0.0 100.0 0.1)
          peak-I (apply max (map #(aget % 1) (:us sol)))
          final (last (:us sol))
          final-R (aget final 2)]
      (is (< 295 peak-I 310)
          (str "peak infected outside expected window: " peak-I))
      (is (< 930 final-R 940)
          (str "final recovered outside expected window: " final-R)))))

;; ============================================================================
;; UWD-based Petri composition
;; ============================================================================

(defn- infection-box
  "Sub-Petri net for one infection reaction: S + I → 2I."
  []
  (let [n (p/petri)
        [n _s] (p/add-species n)
        [n _i] (p/add-species n)
        [n t]  (p/add-transition n)
        [n _]  (p/add-input n 1 t)
        [n _]  (p/add-input n 2 t)
        [n _]  (p/add-output n 2 t)
        [n _]  (p/add-output n 2 t)] n))

(defn- recovery-box
  "Sub-Petri net for one recovery reaction: I → R."
  []
  (let [n (p/petri)
        [n _i] (p/add-species n)
        [n _r] (p/add-species n)
        [n t]  (p/add-transition n)
        [n _]  (p/add-input n 1 t)
        [n _]  (p/add-output n 2 t)] n))

(defn- sir-uwd-composition
  "Compose infection-box and recovery-box along junctions S, I, R via a UWD."
  []
  (let [d        (uwd/uwd)
        [d js]   (uwd/add-junctions d 3)
        [d B1 _] (uwd/add-box-with-ports d (take 2 js))
        [d B2 _] (uwd/add-box-with-ports d (drop 1 js))]
    (p/compose-petri d
                     {B1 (infection-box)  B2 (recovery-box)}
                     {B1 [1 2]            B2 [1 2]})))

(deftest test-composed-petri-has-expected-shape
  (testing "Composing infection and recovery along the I junction yields the
            standard 3-species, 2-transition SIR net"
    (let [composed (sir-uwd-composition)]
      (is (= 3 (count (p/species composed))))
      (is (= 2 (count (p/transitions composed)))))))

(deftest test-composed-petri-matches-direct-construction
  (testing "Composition gives the same multiplicities as a direct SIR build"
    (let [[direct _ _] (sir-direct)
          composed     (sir-uwd-composition)]
      (is (= (p/in-multiplicity  direct) (p/in-multiplicity  composed)))
      (is (= (p/out-multiplicity direct) (p/out-multiplicity composed))))))

(deftest test-compose-petri-no-junctions-is-disjoint-union
  (testing "When the UWD has no junctions with ≥2 ports, compose-petri
            returns the disjoint union of all box nets (no species
            collapse)"
    (let [;; Two independent decay-like boxes; each has a junction of its
          ;; own but they don't share.
          d        (uwd/uwd)
          [d js]   (uwd/add-junctions d 2)
          [d B1 _] (uwd/add-box-with-ports d [(first js)])
          [d B2 _] (uwd/add-box-with-ports d [(second js)])
          ;; Each box has a single-species "do nothing" net (1 species,
          ;; 1 transition, no input arcs — a constant source).
          mk (fn []
               (let [n (p/petri)
                     [n _s] (p/add-species n)
                     [n t]  (p/add-transition n)
                     [n _]  (p/add-output n 1 t)]
                 n))
          composed (p/compose-petri d {B1 (mk) B2 (mk)} {B1 [1] B2 [1]})]
      (is (= 2 (count (p/species composed)))
          "no junction shared between boxes → 2 distinct species")
      (is (= 2 (count (p/transitions composed)))))))

(deftest test-compose-petri-three-port-box
  (testing "A box with 3 ports wired to 3 distinct junctions exposes all
            three of its species to the composite"
    (let [;; One box with 3 species, 1 transition consuming all three.
          net (let [n (p/petri)
                    [n _a] (p/add-species n)
                    [n _b] (p/add-species n)
                    [n _c] (p/add-species n)
                    [n t]  (p/add-transition n)
                    [n _]  (p/add-input n 1 t)
                    [n _]  (p/add-input n 2 t)
                    [n _]  (p/add-input n 3 t)
                    [n _]  (p/add-output n 1 t)]
                n)
          d        (uwd/uwd)
          [d js]   (uwd/add-junctions d 3)
          [d B _]  (uwd/add-box-with-ports d js)
          composed (p/compose-petri d {B net} {B [1 2 3]})]
      (is (= 3 (count (p/species composed))))
      (is (= 1 (count (p/transitions composed)))))))

(deftest test-composed-petri-integrates-identically
  (testing "Direct and composed nets produce the same SIR trajectory"
    (let [[direct inf rec] (sir-direct)
          composed         (sir-uwd-composition)
          N    1000.0
          rates-d {inf (/ 0.3 N) rec 0.1}
          ;; composed's transitions are 1 (infection from box 1) and 2 (recovery from box 2)
          rates-c {1   (/ 0.3 N) 2 0.1}
          f-d (p/petri->derivative direct   rates-d)
          f-c (p/petri->derivative composed rates-c)
          u0  [(- N 1.0) 1.0 0.0]
          sol-d (p/integrate-rk4 f-d u0 0.0 100.0 0.1)
          sol-c (p/integrate-rk4 f-c u0 0.0 100.0 0.1)
          last-d (last (:us sol-d))
          last-c (last (:us sol-c))]
      (dotimes [k 3]
        (is (< (Math/abs (- (aget last-d k) (aget last-c k))) 1e-9))))))
