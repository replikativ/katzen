(ns katzen.datahike-backed-test
  "Tests that the RasterCompilable protocol works transparently on
   DatahikeACSet-backed concepts (Petri nets, reaction networks) without
   any code changes from the VectorACSet path.

   The protocol abstracts over backend: state-labels can be any value
   (vector part-ids are 1-based contiguous ints; datahike eids are
   arbitrary positive ints), and the compile pipeline indexes through
   the layout's `:index-of` map either way. The result is that the
   raster ftm body uses 0..n-1 slot indices regardless of how the
   underlying ACSet stored its parts.

   This test alias requires `:datahike` to be on the classpath; the
   tests skip cleanly when it isn't."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.compile.core :as cc]
            [katzen.petri :as p]
            [katzen.reaction :as rxn]
            [katzen.test-support :as ts]
            [raster.ode :as ode]))

(def datahike-available? ts/datahike-available?)

(defn- dh-acset [schema]
  ((requiring-resolve 'katzen.acset.datahike/datahike-acset) schema))

;; ============================================================================
;; Petri SIR on datahike
;; ============================================================================

(defn- sir-on-backend
  "Build an SIR PetriDynamics. The constructor takes the *acset* (so the
   caller chooses Vector vs Datahike) and threads through the species
   eids correctly. Returns [dynamics initial-state]."
  [acset N beta-N gamma]
  (let [n acset
        [n S] (p/add-species n)
        [n I] (p/add-species n)
        [n R] (p/add-species n)
        [n inf] (p/add-transition n)
        [n rec] (p/add-transition n)
        [n _] (p/add-input  n S inf)
        [n _] (p/add-input  n I inf)
        [n _] (p/add-output n I inf)
        [n _] (p/add-output n I inf)
        [n _] (p/add-input  n I rec)
        [n _] (p/add-output n R rec)]
    [(p/petri-dynamics n {inf beta-N rec gamma})
     [(- N 1.0) 1.0 0.0]]))

(deftest test-datahike-petri-integrates-identically-to-vector
  (if-not datahike-available?
    (ts/skip-notice ":datahike")
    (let [N 1000.0
          beta-N (/ 0.3 N)
          gamma 0.1
          [vec-dyn vec-u0] (sir-on-backend (a/vector-acset p/SchPetri) N beta-N gamma)
          [dh-dyn  dh-u0]  (sir-on-backend (dh-acset p/SchPetri)        N beta-N gamma)
          rhs-v (cc/compile-rhs vec-dyn)
          rhs-d (cc/compile-rhs dh-dyn)
          sol-v (ode/solve (ode/tsit5) (ode/ode-problem rhs-v (double-array vec-u0) 0.0 100.0) 1.0)
          sol-d (ode/solve (ode/tsit5) (ode/ode-problem rhs-d (double-array dh-u0)  0.0 100.0) 1.0)
          fv (vec (last (:us sol-v)))
          fd (vec (last (:us sol-d)))]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth fv k) (nth fd k)))
                     (max 1e-6 (Math/abs (nth fv k))))]
          (is (< rel 1e-10)
              (str "slot " k ": vec=" (nth fv k) " dh=" (nth fd k))))))))

;; ============================================================================
;; ReactionDynamics on datahike
;; ============================================================================

(defn- mm-on-backend
  "Michaelis-Menten: S → P at rate Vmax·S/(Km+S). Returns [dynamics u0]."
  [acset Vmax Km]
  (let [n acset
        [n S] (rxn/add-species n)
        [n P] (rxn/add-species n)
        [n R] (rxn/add-reaction n)
        [n _] (rxn/add-substrate n S R)
        [n _] (rxn/add-product   n P R)]
    [(rxn/reaction-dynamics n {R {:type :michaelis-menten :Vmax Vmax :Km Km :substrate S}})
     [10.0 0.0]]))

(deftest test-datahike-reaction-matches-vector
  (if-not datahike-available?
    (ts/skip-notice ":datahike")
    (let [[v-dyn v-u0] (mm-on-backend (a/vector-acset rxn/SchReactionNetwork) 2.0 5.0)
          [d-dyn d-u0] (mm-on-backend (dh-acset rxn/SchReactionNetwork)       2.0 5.0)
          rhs-v (cc/compile-rhs v-dyn)
          rhs-d (cc/compile-rhs d-dyn)
          sol-v (ode/solve (ode/tsit5) (ode/ode-problem rhs-v (double-array v-u0) 0.0 10.0) 0.1)
          sol-d (ode/solve (ode/tsit5) (ode/ode-problem rhs-d (double-array d-u0) 0.0 10.0) 0.1)
          fv (vec (last (:us sol-v)))
          fd (vec (last (:us sol-d)))]
      (dotimes [k 2]
        (let [rel (/ (Math/abs (- (nth fv k) (nth fd k)))
                     (max 1e-6 (Math/abs (nth fv k))))]
          (is (< rel 1e-10)))))))

;; ============================================================================
;; State layout uses datahike eids as labels
;; ============================================================================

(deftest test-datahike-layout-labels-are-eids
  (if-not datahike-available?
    (ts/skip-notice ":datahike")
    (let [[dyn _] (sir-on-backend (dh-acset p/SchPetri) 1000.0 0.0003 0.1)
          layout (cc/layout-of dyn)]
      (is (= 3 (:size layout)))
      (is (every? pos? (keys (:index-of layout)))
          "datahike labels are positive eids")
      (is (= #{0 1 2} (set (vals (:index-of layout))))
          "slot indices are 0..n-1 regardless of label space"))))
