(ns katzen.uwd-test
  "Tests for undirected wiring diagrams and the relations-algebra oapply.

   We assert two kinds of properties: (1) the ACSet shape after each
   constructor matches what we expect, and (2) the oapply on the
   relations algebra yields the standard relational composition over a
   small finite type, validated against a hand-computed reference."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.uwd :as uwd]))

;; ============================================================================
;; SchUWD + constructors
;; ============================================================================

(deftest test-empty-uwd
  (let [d (uwd/uwd)]
    (is (a/acset? d))
    (is (= 0 (uwd/nboxes d)))
    (is (= 0 (uwd/njunctions d)))
    (is (= 0 (uwd/nports d)))
    (is (= 0 (uwd/nouter-ports d)))
    (is (= uwd/SchUWD (a/schema d)))))

(deftest test-add-box-with-ports
  (testing "Add a box with 3 ports each attached to a different junction"
    (let [d (uwd/uwd)
          [d js] (uwd/add-junctions d 3)
          [d b ports] (uwd/add-box-with-ports d js)]
      (is (= 1 (uwd/nboxes d)))
      (is (= 3 (uwd/nports d)))
      (is (= 3 (count ports)))
      (is (every? #(= b (uwd/port-box d %)) ports))
      (is (= js (uwd/box-junctions d b))))))

(deftest test-add-outer-port
  (let [d (uwd/uwd)
        [d j] (uwd/add-junction d)
        [d op] (uwd/add-outer-port d j)]
    (is (= 1 (uwd/nouter-ports d)))
    (is (= j (uwd/outer-junction d op)))))

(deftest test-junction-ports-accessor
  (testing "junction-ports returns every port on a given junction"
    (let [d (uwd/uwd)
          [d j] (uwd/add-junction d)
          [d _]  (uwd/add-box-with-ports d [j])
          [d _]  (uwd/add-box-with-ports d [j])]
      (is (= 2 (count (uwd/junction-ports d j)))))))

;; ============================================================================
;; oapply on the relations algebra
;; ============================================================================

(deftest test-oapply-identity-composition
  (testing "R(x,y) ∧ S(y,z) with R = S = identity on {0,1} yields the
            identity-equality on three junctions"
    (let [d (uwd/uwd)
          [d js] (uwd/add-junctions d 3)
          [d b1 _] (uwd/add-box-with-ports d (take 2 js))
          [d b2 _] (uwd/add-box-with-ports d (drop 1 js))
          id-rel  #{[0 0] [1 1]}
          result  (uwd/oapply-relations d {b1 id-rel b2 id-rel} 2)]
      (is (= #{[0 0 0] [1 1 1]} result)))))

(deftest test-oapply-transitive-composition
  (testing "R(x,y) ∧ S(y,z) with R = S = ≤ on {0,1,2} yields x ≤ y ≤ z"
    (let [d (uwd/uwd)
          [d js] (uwd/add-junctions d 3)
          [d b1 _] (uwd/add-box-with-ports d (take 2 js))
          [d b2 _] (uwd/add-box-with-ports d (drop 1 js))
          le-rel  (set (for [a (range 3) b (range 3) :when (<= a b)] [a b]))
          expected (set (for [a (range 3) b (range 3) c (range 3)
                              :when (and (<= a b) (<= b c))] [a b c]))
          result  (uwd/oapply-relations d {b1 le-rel b2 le-rel} 3)]
      (is (= expected result))
      (is (= 10 (count result))))))

(deftest test-oapply-empty-box-zeros-everything
  (testing "If one box has the empty relation the composite is empty"
    (let [d (uwd/uwd)
          [d js] (uwd/add-junctions d 2)
          [d b1 _] (uwd/add-box-with-ports d js)
          [d b2 _] (uwd/add-box-with-ports d js)
          result  (uwd/oapply-relations d {b1 #{[0 0] [1 1]} b2 #{}} 2)]
      (is (= #{} result)))))

(deftest test-oapply-triangle
  (testing "Triangle R(x,y) ∧ S(y,z) ∧ T(z,x) on {0,1}: count of consistent
            assignments matches a direct enumeration"
    (let [d (uwd/uwd)
          [d js] (uwd/add-junctions d 3)
          [x y z] js
          rel #{[0 0] [0 1] [1 1]}
          [d b1 _] (uwd/add-box-with-ports d [x y])
          [d b2 _] (uwd/add-box-with-ports d [y z])
          [d b3 _] (uwd/add-box-with-ports d [z x])
          result   (uwd/oapply-relations d {b1 rel b2 rel b3 rel} 2)
          expected (set (for [a (range 2) b (range 2) c (range 2)
                              :when (and (rel [a b]) (rel [b c]) (rel [c a]))]
                          [a b c]))]
      (is (= expected result)))))
