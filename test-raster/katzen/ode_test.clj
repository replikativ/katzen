(ns katzen.ode-test
  "Tests the vector-field / raw-field RasterCompilables against the
   textbook Lotka–Volterra predator–prey model — the case that does NOT
   reduce to a clean mass-action Petri net.

   Three things must agree:
     - a standalone symbolic `vector-field` on the Clojure and raster paths,
     - the same LV decomposed into 3 boxes composed via `oapply` with
       `vector-field` leaves,
     - and again with opaque `raw-field` leaves.
   All integrate to the same trajectory and conserve the LV first integral."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.ode :as ode]
            [katzen.petri :as p]
            [katzen.uwd :as uwd]
            [katzen.uwd.dynamics :as ud]
            ;; raster namespaces must be loaded so `compile-rhs`'s emitted
            ;; `raster.core/ftm` / `raster.numeric` / `raster.arrays` forms resolve.
            [raster.core]
            [raster.numeric]
            [raster.arrays]))

;; LV with α=1.1 β=0.4 δ=0.1 γ=0.4:
;;   dr/dt = α r − β r f
;;   df/dt = δ r f − γ f
(def ALPHA 1.1) (def BETA 0.4) (def DELTA 0.1) (def GAMMA 0.4)

(defn- lv-field []
  (ode/vector-field
   {:states '[r f]
    :params {'alpha ALPHA 'beta BETA 'delta DELTA 'gamma GAMMA}
    :field  '{r (- (* alpha r) (* beta r f))
              f (- (* delta r f) (* gamma f))}}))

;; LV first integral V = δr − γ·ln r + βf − α·ln f is conserved along orbits.
(defn- V [[r f]]
  (+ (- (* DELTA r) (* GAMMA (Math/log r)))
     (- (* BETA f)  (* ALPHA (Math/log f)))))

(defn- end-state [rhs]
  (-> (p/integrate-rk4 rhs (double-array [10.0 10.0]) 0.0 20.0 0.001)
      :us last vec))

(defn- v-drift [rhs]
  (let [{:keys [us]} (p/integrate-rk4 rhs (double-array [10.0 10.0]) 0.0 20.0 0.001)]
    (Math/abs (double (- (V (vec (first us))) (V (vec (last us))))))))

(defn- ≈ [a b] (< (Math/abs (double (- a b))) 1e-6))
(defn- states≈ [u v] (every? true? (map ≈ u v)))

;; ----------------------------------------------------------------------------

(deftest standalone-clojure-and-raster-agree
  (testing "the symbolic field compiles to both bodies and they integrate alike"
    (let [vf      (lv-field)
          clj-end (end-state (cc/compile-clojure-rhs vf))
          ras-end (end-state (cc/compile-rhs vf))]
      (is (states≈ clj-end ras-end) "clojure path == raster path")
      (is (< (v-drift (cc/compile-clojure-rhs vf)) 1e-6)
          "LV first integral conserved → the orbit is correct"))))

;; ----------------------------------------------------------------------------
;; Decompose LV into 3 boxes sharing the R and F populations via junctions.

(defn- compose-lv [growth predation death]
  (let [d            (uwd/uwd)
        [d [jR jF]]  (uwd/add-junctions d 2)
        [d Bg _]     (uwd/add-box-with-ports d [jR])      ; growth touches R
        [d Bp _]     (uwd/add-box-with-ports d [jR jF])   ; predation touches R,F
        [d Bd _]     (uwd/add-box-with-ports d [jF])      ; death touches F
        [d _]        (uwd/add-outer-port d jR)
        [d _]        (uwd/add-outer-port d jF)]
    (ud/oapply d {Bg (ud/from-compilable growth    '[r])
                  Bp (ud/from-compilable predation '[r f])
                  Bd (ud/from-compilable death     '[f])})))

(deftest vector-field-composite-equals-standalone
  (testing "composing 3 symbolic boxes == the monolithic field; shared populations merged"
    (let [crs (compose-lv
               (ode/vector-field {:states '[r]   :params {'alpha ALPHA} :field '{r (* alpha r)}})
               (ode/vector-field {:states '[r f] :params {'beta BETA 'delta DELTA}
                                  :field '{r (- (* beta r f)) f (* delta r f)}})
               (ode/vector-field {:states '[f]   :params {'gamma GAMMA} :field '{f (- (* gamma f))}}))]
      (is (= 2 (:size (cc/layout-of crs)))
          "R and F identified across boxes — 2 states, not 4")
      (is (states≈ (end-state (cc/compile-clojure-rhs crs))
                   (end-state (cc/compile-clojure-rhs (lv-field))))
          "composite trajectory == standalone")
      (is (< (v-drift (cc/compile-clojure-rhs crs)) 1e-6)))))

(deftest raw-field-composite-equals-standalone
  (testing "opaque-closure boxes (the AlgebraicDynamics analog) compose identically"
    (let [crs (compose-lv
               (ode/raw-field {:states '[r]   :dynamics (fn [[r] _]   [(* ALPHA r)])})
               (ode/raw-field {:states '[r f] :dynamics (fn [[r f] _] [(- (* BETA r f)) (* DELTA r f)])})
               (ode/raw-field {:states '[f]   :dynamics (fn [[f] _]   [(- (* GAMMA f))])}))]
      (is (= 2 (:size (cc/layout-of crs))))
      (is (states≈ (end-state (cc/compile-clojure-rhs crs))
                   (end-state (cc/compile-clojure-rhs (lv-field))))
          "raw-field composite == standalone")
      (is (< (v-drift (cc/compile-clojure-rhs crs)) 1e-6)))))

(deftest raw-field-has-no-raster-body
  (testing "an opaque closure can't be inlined into raster code"
    (is (thrown-with-msg? Exception #"no raster body"
                          (cc/compile-rhs (ode/raw-field {:states '[x]
                                                          :dynamics (fn [[x] _] [(- x)])}))))))

(deftest vector-field-rejects-unknown-state
  (is (thrown-with-msg? Exception #"not a declared state"
                        (ode/vector-field {:states '[r] :field '{q (* 2 r)}}))))
