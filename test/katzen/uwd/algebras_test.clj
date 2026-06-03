(ns katzen.uwd.algebras-test
  "Tests for the generic UWD algebra protocol and its scalar-multiplicative
   instances (relations and counting)."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.uwd :as uwd]
            [katzen.uwd.algebras :as alg]))

(defn- triangle-uwd
  "UWD for R(x,y) ∧ S(y,z) ∧ T(z,x): three boxes, three junctions, six
   ports total. Returns the diagram plus the box IDs in build order."
  []
  (let [d (uwd/uwd)
        [d js] (uwd/add-junctions d 3)
        [x y z] js
        [d b1 _] (uwd/add-box-with-ports d [x y])
        [d b2 _] (uwd/add-box-with-ports d [y z])
        [d b3 _] (uwd/add-box-with-ports d [z x])]
    [d b1 b2 b3]))

;; ============================================================================
;; RelationsAlgebra
;; ============================================================================

(deftest test-relations-algebra-identity
  (testing "All-identity relations: composite identifies all three junctions"
    (let [[d b1 b2] (let [[d b1 b2 _] (triangle-uwd)] [d b1 b2])
          id-rel #{[0 0] [1 1]}
          ;; Two-box version: drop b3 by giving it a full universal relation
          full   (set (for [a (range 2) b (range 2)] [a b]))
          result (alg/oapply (alg/relations 2) d {b1 id-rel, b2 id-rel,
                                                  (nth (triangle-uwd) 3) full})]
      ;; Only (0,0,0) and (1,1,1) satisfy all three boxes.
      (is (= #{[0 0 0] [1 1 1]} result)))))

(deftest test-relations-matches-direct-enumeration
  (testing "Composite by oapply matches a brute-force enumeration check"
    (let [[d b1 b2 b3] (triangle-uwd)
          rel #{[0 0] [0 1] [1 1]}
          result   (alg/oapply (alg/relations 2) d {b1 rel b2 rel b3 rel})
          expected (set (for [a (range 2) b (range 2) c (range 2)
                              :when (and (rel [a b]) (rel [b c]) (rel [c a]))]
                          [a b c]))]
      (is (= expected result)))))

(deftest test-relations-empty-box-yields-empty
  (let [[d b1 b2 b3] (triangle-uwd)
        result (alg/oapply (alg/relations 2) d {b1 #{} b2 #{[0 0]} b3 #{[1 1]}})]
    (is (= #{} result))))

;; ============================================================================
;; CountingAlgebra
;; ============================================================================

(deftest test-counting-matches-product-of-counts
  (testing "Counting algebra returns per-assignment products of box counts"
    (let [d        (uwd/uwd)
          [d js]   (uwd/add-junctions d 2)
          [x y]    js
          [d b1 _] (uwd/add-box-with-ports d [x y])
          [d b2 _] (uwd/add-box-with-ports d [x y])
          ms1 {[0 0] 2 [0 1] 1 [1 1] 1}
          ms2 {[0 0] 3 [0 1] 2 [1 1] 4}
          result (alg/oapply (alg/counting 2) d {b1 ms1 b2 ms2})]
      ;; For each (x, y) the product is ms1[(x,y)] · ms2[(x,y)].
      (is (= {[0 0] 6     ; 2·3
              [0 1] 2     ; 1·2
              [1 1] 4}    ; 1·4
             result))
      ;; (1, 0) is missing from both → score 0 → dropped.
      (is (not (contains? result [1 0]))))))

(deftest test-counting-as-relations-when-all-counts-are-one
  (testing "If every box-multiset is 0/1 valued, counting reduces to
            relations (every present assignment has count 1)"
    (let [d        (uwd/uwd)
          [d js]   (uwd/add-junctions d 2)
          [x y]    js
          [d b1 _] (uwd/add-box-with-ports d [x y])
          [d b2 _] (uwd/add-box-with-ports d [x y])
          ms {[0 0] 1 [1 1] 1}]
      (is (= {[0 0] 1 [1 1] 1}
             (alg/oapply (alg/counting 2) d {b1 ms b2 ms}))))))
