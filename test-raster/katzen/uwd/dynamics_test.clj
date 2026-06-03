(ns katzen.uwd.dynamics-test
  "Tests the UWD-dynamics oapply against the existing Petri composition
   path. Same SIR scenario:
     - Direct Petri    (katzen.petri)
     - Petri composed via UWD (katzen.petri/compose-petri)
     - CRS composed via UWD   (katzen.uwd.dynamics/oapply)
   All three must produce equivalent ODEs and integrate to the same
   trajectory."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.petri :as p]
            [katzen.uwd :as uwd]
            [katzen.uwd.dynamics :as ud]
            [raster.ode :as ode]))

;; ============================================================================
;; Builders shared with the rest of the test suite
;; ============================================================================

(defn- infection-net []
  (let [n (p/petri)
        [n _s] (p/add-species n)
        [n _i] (p/add-species n)
        [n t]  (p/add-transition n)
        [n _]  (p/add-input  n 1 t)
        [n _]  (p/add-input  n 2 t)
        [n _]  (p/add-output n 2 t)
        [n _]  (p/add-output n 2 t)]
    n))

(defn- recovery-net []
  (let [n (p/petri)
        [n _i] (p/add-species n)
        [n _r] (p/add-species n)
        [n t]  (p/add-transition n)
        [n _]  (p/add-input  n 1 t)
        [n _]  (p/add-output n 2 t)]
    n))

(defn- sir-composite-crs
  "Build the SIR CRS composite. Returns the CRS plus the two box ids
   so tests can inspect the layout."
  [beta-N gamma]
  (let [d        (uwd/uwd)
        [d js]   (uwd/add-junctions d 3)
        [d B1 _] (uwd/add-box-with-ports d (take 2 js))
        [d B2 _] (uwd/add-box-with-ports d (drop 1 js))
        b1-crs   (ud/from-compilable
                  (p/petri-dynamics (infection-net) {1 beta-N})
                  [1 2])
        b2-crs   (ud/from-compilable
                  (p/petri-dynamics (recovery-net)  {1 gamma})
                  [1 2])]
    {:crs (ud/oapply d {B1 b1-crs B2 b2-crs})
     :B1 B1 :B2 B2}))

;; ============================================================================
;; from-compilable
;; ============================================================================

(deftest test-from-compilable-rejects-unknown-port-state
  (let [petri-with-rates (p/petri-dynamics (infection-net) {1 0.001})]
    (is (thrown-with-msg? Exception #"port-state not in layout"
                          (ud/from-compilable petri-with-rates [1 2 99])))))

;; ============================================================================
;; oapply produces correct shape
;; ============================================================================

(deftest test-composite-layout-merges-shared-junctions
  (testing "Box 1's species 2 and Box 2's species 1 both wire to the I junction,
            so the composite layout maps them to the same slot"
    (let [{:keys [crs B1 B2]} (sir-composite-crs 0.0003 0.1)
          layout (cc/layout-of crs)]
      (is (= 3 (:size layout)) "S, I, R = 3 classes")
      (is (= (cc/slot layout [B1 2]) (cc/slot layout [B2 1]))
          "I-from-infection ≡ I-from-recovery")
      (is (apply distinct?
                 (map #(cc/slot layout %) [[B1 1] [B1 2] [B2 2]]))
          "S, I, R end up in distinct slots"))))

;; ============================================================================
;; Composite integrates identically to direct Petri SIR
;; ============================================================================

(defn- direct-sir-rhs
  "Direct SIR Petri net RHS via the protocol (PetriDynamics)."
  [beta-N gamma]
  (let [n (p/petri)
        [n _s]  (p/add-species n)
        [n _i]  (p/add-species n)
        [n _r]  (p/add-species n)
        [n inf] (p/add-transition n)
        [n rec] (p/add-transition n)
        [n _]   (p/add-input  n 1 inf)
        [n _]   (p/add-input  n 2 inf)
        [n _]   (p/add-output n 2 inf)
        [n _]   (p/add-output n 2 inf)
        [n _]   (p/add-input  n 2 rec)
        [n _]   (p/add-output n 3 rec)]
    (cc/compile-rhs (p/petri-dynamics n {inf beta-N rec gamma}))))

(deftest test-oapply-matches-direct-petri-trajectory
  (testing "CRS-composed SIR and direct PetriDynamics SIR integrate to the
            same trajectory under Tsit5"
    (let [beta-N (/ 0.3 1000.0)
          gamma 0.1
          u0    (double-array [999.0 1.0 0.0])
          rhs-d (direct-sir-rhs beta-N gamma)
          rhs-c (cc/compile-rhs (:crs (sir-composite-crs beta-N gamma)))
          sol-d (ode/solve (ode/tsit5) (ode/ode-problem rhs-d u0 0.0 100.0) 1.0)
          sol-c (ode/solve (ode/tsit5) (ode/ode-problem rhs-c (aclone u0) 0.0 100.0) 1.0)
          fa (vec (last (:us sol-d)))
          fb (vec (last (:us sol-c)))]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth fa k) (nth fb k)))
                     (max 1e-6 (Math/abs (nth fa k))))]
          (is (< rel 1e-10)
              (str "slot " k ": direct=" (nth fa k) " composed=" (nth fb k))))))))

;; ============================================================================
;; Conservation: total population stays at N
;; ============================================================================

(deftest test-composite-conserves-population
  (testing "Composite SIR conserves total population to within float drift"
    (let [{:keys [crs]} (sir-composite-crs (/ 0.3 1000.0) 0.1)
          rhs  (cc/compile-rhs crs)
          u0   (double-array [999.0 1.0 0.0])
          prob (ode/ode-problem rhs u0 0.0 100.0)
          sol  (ode/solve (ode/tsit5) prob 1.0)]
      (doseq [u (:us sol)]
        (let [total (+ (aget u 0) (aget u 1) (aget u 2))]
          (is (< (Math/abs (- total 1000.0)) 1e-6)))))))

;; ============================================================================
;; Clojure fallback: same trajectory under in-house RK4
;; ============================================================================

(deftest test-clojure-fallback-tracks-raster
  (testing "compile-clojure-rhs and compile-rhs produce trajectories that
            agree under their respective integrators within 1%"
    (let [{:keys [crs]} (sir-composite-crs (/ 0.3 1000.0) 0.1)
          rhs-c (cc/compile-clojure-rhs crs)
          rhs-r (cc/compile-rhs crs)
          u0    (double-array [999.0 1.0 0.0])
          sol-c (p/integrate-rk4 rhs-c [999.0 1.0 0.0] 0.0 100.0 0.05)
          sol-r (ode/solve (ode/tsit5) (ode/ode-problem rhs-r u0 0.0 100.0) 1.0)
          fa (vec (last (:us sol-c)))
          fb (vec (last (:us sol-r)))]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth fa k) (nth fb k)))
                     (max 1e-6 (Math/abs (nth fa k))))]
          (is (< rel 0.01)))))))

;; ============================================================================
;; Composite exposes ports for the outer interface
;; ============================================================================

(deftest test-composite-port-states
  (testing "Outer-ports become composite port-states by tracing through their
            junction's inner ports"
    (let [d        (uwd/uwd)
          [d js]   (uwd/add-junctions d 3)
          [d B1 _] (uwd/add-box-with-ports d (take 2 js))
          [d B2 _] (uwd/add-box-with-ports d (drop 1 js))
          ;; Add two outer ports — one on junction S, one on junction R.
          [d _]    (uwd/add-outer-port d (first js))
          [d _]    (uwd/add-outer-port d (last  js))
          crs (ud/oapply d
                         {B1 (ud/from-compilable
                              (p/petri-dynamics (infection-net) {1 0.0003}) [1 2])
                          B2 (ud/from-compilable
                              (p/petri-dynamics (recovery-net)  {1 0.1})    [1 2])})]
      (is (= 2 (count (:port-states crs))))
      (is (= [[B1 1] [B2 2]] (:port-states crs))
          "outer ports map to box-1's species 1 (S) and box-2's species 2 (R)"))))
