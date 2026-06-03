(ns katzen.compile.lint-test
  "Tests for the static lint pass over raster bodies."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.compile.lint :as lint]))

;; ============================================================================
;; Hand-crafted RasterCompilable values
;; ============================================================================
;;
;; The protocol takes minimal shape: `state-layout`, `raster-body`,
;; `clojure-body`. We can implement it on a defrecord for testing without
;; needing the full PetriDynamics machinery.

(defrecord MockRC [layout body-form]
  cc/RasterCompilable
  (-state-layout [_] layout)
  (-raster-body  [_ _] body-form)
  (-clojure-body [_ _] (fn [_du _u _t])))

(defn- mock [size body-form]
  (->MockRC (cc/state-layout (mapv #(keyword (str "s" %)) (range size)))
            body-form))

;; ============================================================================
;; Clean cases
;; ============================================================================

(deftest test-empty-body-lints-clean
  (is (= :ok (lint/lint-compilable (mock 3 [])))))

(deftest test-in-range-aset-clean
  (let [body [`(raster.arrays/aset ~'du 0 0.0)
              `(raster.arrays/aset ~'du 1 0.0)
              `(raster.arrays/aset ~'du 2 0.0)]]
    (is (= :ok (lint/lint-compilable (mock 3 body))))))

(deftest test-aget-inside-aset-clean
  (let [body [`(raster.arrays/aset ~'du 0
                                   (raster.numeric/+ (raster.arrays/aget ~'u 0)
                                                     (raster.arrays/aget ~'u 1)))]]
    (is (= :ok (lint/lint-compilable (mock 2 body))))))

;; ============================================================================
;; Violations
;; ============================================================================

(deftest test-negative-index-flagged
  (let [body [`(raster.arrays/aset ~'du -1 0.0)]
        result (lint/lint-compilable (mock 3 body))]
    (is (vector? result))
    (is (= 1 (count result)))
    (is (= :out-of-bounds (:issue (first result))))
    (is (= -1 (:index (first result))))
    (is (= 3 (:layout-size (first result))))))

(deftest test-index-equal-to-size-flagged
  (testing "Index 3 in a size-3 layout (valid indices 0..2) is out of bounds"
    (let [body [`(raster.arrays/aset ~'du 3 0.0)]
          result (lint/lint-compilable (mock 3 body))]
      (is (= 1 (count result))))))

(deftest test-aget-out-of-bounds-flagged
  (let [body [`(raster.arrays/aset ~'du 0 (raster.arrays/aget ~'u 5))]
        result (lint/lint-compilable (mock 3 body))]
    (is (= 1 (count result)))
    (is (= :aget (:op (first result))))))

(deftest test-multiple-violations-all-collected
  (let [body [`(raster.arrays/aset ~'du 99 0.0)
              `(raster.arrays/aset ~'du -1 (raster.arrays/aget ~'u 50))]
        result (lint/lint-compilable (mock 3 body))]
    (is (= 3 (count result))
        "two asets + one aget = three out-of-bounds references")))

;; ============================================================================
;; Strict variant
;; ============================================================================

(deftest test-lint-bang-passes-through-on-clean
  (let [m (mock 1 [`(raster.arrays/aset ~'du 0 0.0)])]
    (is (identical? m (lint/lint! m)))))

(deftest test-lint-bang-throws-on-violation
  (let [m (mock 1 [`(raster.arrays/aset ~'du 5 0.0)])]
    (is (thrown-with-msg? Exception #"lint failed"
                          (lint/lint! m)))))

;; ============================================================================
;; Integration with a real concept (PetriDynamics)
;; ============================================================================

(deftest test-real-petri-dynamics-lints-clean
  (testing "PetriDynamics produced by the standard pipeline emits valid indices"
    (require 'katzen.petri)
    (let [petri-ns (find-ns 'katzen.petri)
          petri    (ns-resolve petri-ns 'petri)
          add-sp   (ns-resolve petri-ns 'add-species)
          add-tr   (ns-resolve petri-ns 'add-transition)
          add-in   (ns-resolve petri-ns 'add-input)
          add-out  (ns-resolve petri-ns 'add-output)
          petri-dyn (ns-resolve petri-ns 'petri-dynamics)
          n       (petri)
          [n s]   (add-sp n)
          [n i]   (add-sp n)
          [n _r]  (add-sp n)
          [n inf] (add-tr n)
          [n rec] (add-tr n)
          [n _]   (add-in n s inf)
          [n _]   (add-in n i inf)
          [n _]   (add-out n i inf)
          [n _]   (add-in n i rec)
          [n _]   (add-out n 3 rec)
          dyn (petri-dyn n {inf 0.001 rec 0.1})]
      (is (= :ok (lint/lint-compilable dyn))))))
