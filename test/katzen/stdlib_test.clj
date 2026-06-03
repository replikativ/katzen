(ns katzen.stdlib-test
  "Tests for standard theory library."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.stdlib.core :as stdlib]
            [katzen.stdlib.examples :as examples]
            [katzen.core :as core]
            [katzen.presentation :as pres]
))

;;; ============================================================================
;;; Theory Definition Tests
;;; ============================================================================

(deftest test-thgraph-definition
  (testing "ThGraph theory is properly defined"
    (is (core/gat? stdlib/ThGraph))
    (is (= 'ThGraph (:name stdlib/ThGraph)))

    ;; Should have 2 types: V and E
    (is (= 2 (count (:type-constructors stdlib/ThGraph))))
    (let [type-names (map #(-> % :type :head :name) (:type-constructors stdlib/ThGraph))]
      (is (some #(= 'V %) type-names))
      (is (some #(= 'E %) type-names)))

    ;; V should have no context (primitive type)
    (let [v-type (first (:type-constructors stdlib/ThGraph))
          ctx (:ctx v-type)]
      (is (= 0 (count (:idents ctx))))
      (is (= 0 (count (:types ctx)))))

    ;; E should have context [src V, tgt V] (dependent type)
    (let [e-type (second (:type-constructors stdlib/ThGraph))]
      (is (= 2 (count (:ctx e-type)))))))

(deftest test-thcategory-definition
  (testing "ThCategory theory is properly defined"
    (is (core/gat? stdlib/ThCategory))
    (is (= 'ThCategory (:name stdlib/ThCategory)))

    ;; Should have 2 types: Ob and Hom
    (is (= 2 (count (:type-constructors stdlib/ThCategory))))
    (let [type-names (map #(-> % :type :head :name) (:type-constructors stdlib/ThCategory))]
      (is (some #(= 'Ob %) type-names))
      (is (some #(= 'Hom %) type-names)))

    ;; Should have 2 term constructors: compose and id
    (is (= 2 (count (:term-constructors stdlib/ThCategory))))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors stdlib/ThCategory))]
      (is (some #(= 'compose %) term-names))
      (is (some #(= 'id %) term-names)))

    ;; Should have 3 axioms: assoc, id-left, id-right
    (is (= 3 (count (:axioms stdlib/ThCategory))))
    (is (some #(= 'assoc (:name %)) (:axioms stdlib/ThCategory)))
    (is (some #(= 'id-left (:name %)) (:axioms stdlib/ThCategory)))
    (is (some #(= 'id-right (:name %)) (:axioms stdlib/ThCategory)))))

(deftest test-thmonoid-definition
  (testing "ThMonoid theory is properly defined"
    (is (core/gat? stdlib/ThMonoid))
    (is (= 'ThMonoid (:name stdlib/ThMonoid)))

    ;; Should have 1 type: El
    (is (= 1 (count (:type-constructors stdlib/ThMonoid))))
    (let [type-names (map #(-> % :type :head :name) (:type-constructors stdlib/ThMonoid))]
      (is (some #(= 'El %) type-names)))

    ;; Should have 2 term constructors: mul and unit
    (is (= 2 (count (:term-constructors stdlib/ThMonoid))))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors stdlib/ThMonoid))]
      (is (some #(= 'mul %) term-names))
      (is (some #(= 'unit %) term-names)))

    ;; Should have 3 axioms: assoc, unit-left, unit-right
    (is (= 3 (count (:axioms stdlib/ThMonoid))))
    (is (some #(= 'assoc (:name %)) (:axioms stdlib/ThMonoid)))
    (is (some #(= 'unit-left (:name %)) (:axioms stdlib/ThMonoid)))
    (is (some #(= 'unit-right (:name %)) (:axioms stdlib/ThMonoid)))))

(deftest test-thgroup-definition
  (testing "ThGroup theory is properly defined"
    (is (core/gat? stdlib/ThGroup))
    (is (= 'ThGroup (:name stdlib/ThGroup)))

    ;; Should have 1 type: El
    (is (= 1 (count (:type-constructors stdlib/ThGroup))))
    (let [type-names (map #(-> % :type :head :name) (:type-constructors stdlib/ThGroup))]
      (is (some #(= 'El %) type-names)))

    ;; Should have 3 term constructors: mul, unit, inv
    (is (= 3 (count (:term-constructors stdlib/ThGroup))))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors stdlib/ThGroup))]
      (is (some #(= 'mul %) term-names))
      (is (some #(= 'unit %) term-names))
      (is (some #(= 'inv %) term-names)))

    ;; Should have 5 axioms: monoid axioms + group axioms
    (is (= 5 (count (:axioms stdlib/ThGroup))))
    (is (some #(= 'assoc (:name %)) (:axioms stdlib/ThGroup)))
    (is (some #(= 'unit-left (:name %)) (:axioms stdlib/ThGroup)))
    (is (some #(= 'unit-right (:name %)) (:axioms stdlib/ThGroup)))
    (is (some #(= 'inv-left (:name %)) (:axioms stdlib/ThGroup)))
    (is (some #(= 'inv-right (:name %)) (:axioms stdlib/ThGroup)))))

(deftest test-thsymmetricmonoidalcategory-definition
  (testing "ThSymmetricMonoidalCategory theory is properly defined"
    (is (core/gat? stdlib/ThSymmetricMonoidalCategory))
    (is (= 'ThSymmetricMonoidalCategory (:name stdlib/ThSymmetricMonoidalCategory)))

    ;; Should have 2 types: Ob and Hom
    (is (= 2 (count (:type-constructors stdlib/ThSymmetricMonoidalCategory))))
    (let [type-names (map #(-> % :type :head :name) (:type-constructors stdlib/ThSymmetricMonoidalCategory))]
      (is (some #(= 'Ob %) type-names))
      (is (some #(= 'Hom %) type-names)))

    ;; Should have 9 term constructors: compose, id, otimes, munit, otimes-hom, associator, left-unitor, right-unitor, braid
    (is (= 9 (count (:term-constructors stdlib/ThSymmetricMonoidalCategory))))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors stdlib/ThSymmetricMonoidalCategory))]
      (is (some #(= 'compose %) term-names))
      (is (some #(= 'id %) term-names))
      (is (some #(= 'otimes %) term-names))
      (is (some #(= 'munit %) term-names))
      (is (some #(= 'otimes-hom %) term-names))
      (is (some #(= 'associator %) term-names))
      (is (some #(= 'left-unitor %) term-names))
      (is (some #(= 'right-unitor %) term-names))
      (is (some #(= 'braid %) term-names)))))

;;; ============================================================================
;;; Theory Utility Function Tests
;;; ============================================================================

(deftest test-list-theories
  (testing "list-theories returns all standard theories"
    (let [theories (stdlib/list-theories)]
      (is (= 6 (count theories)))
      (is (some #(= 'ThGraph %) theories))
      (is (some #(= 'ThCategory %) theories))
      (is (some #(= 'ThSchema %) theories))
      (is (some #(= 'ThSymmetricMonoidalCategory %) theories))
      (is (some #(= 'ThMonoid %) theories))
      (is (some #(= 'ThGroup %) theories)))))

(deftest test-theory-summary
  (testing "theory-summary returns correct metadata for ThGraph"
    (let [summary (stdlib/theory-summary 'ThGraph)]
      (is (= 'ThGraph (:name summary)))
      (is (= 2 (:types summary)))
      (is (= 0 (:terms summary)))
      (is (= 0 (:axioms summary)))))

  (testing "theory-summary returns correct metadata for ThCategory"
    (let [summary (stdlib/theory-summary 'ThCategory)]
      (is (= 'ThCategory (:name summary)))
      (is (= 2 (:types summary)))
      (is (= 2 (:terms summary)))
      (is (= 3 (:axioms summary)))))

  (testing "theory-summary returns correct metadata for ThMonoid"
    (let [summary (stdlib/theory-summary 'ThMonoid)]
      (is (= 'ThMonoid (:name summary)))
      (is (= 1 (:types summary)))
      (is (= 2 (:terms summary)))
      (is (= 3 (:axioms summary)))))

  (testing "theory-summary returns correct metadata for ThGroup"
    (let [summary (stdlib/theory-summary 'ThGroup)]
      (is (= 'ThGroup (:name summary)))
      (is (= 1 (:types summary)))
      (is (= 3 (:terms summary)))
      (is (= 5 (:axioms summary))))))

;;; ============================================================================
;;; Example Presentation Tests
;;; ============================================================================

(deftest test-triangle-graph
  (testing "TriangleGraph presentation is valid"
    (is (pres/presentation? examples/TriangleGraph))
    (is (= stdlib/ThGraph (:theory examples/TriangleGraph)))

    ;; Should have 6 generators: 3 vertices + 3 edges
    (is (= 6 (count (:generators examples/TriangleGraph))))

    ;; Check for specific generators
    (is (contains? (:generators examples/TriangleGraph) 'v1))
    (is (contains? (:generators examples/TriangleGraph) 'v2))
    (is (contains? (:generators examples/TriangleGraph) 'v3))
    (is (contains? (:generators examples/TriangleGraph) 'e12))
    (is (contains? (:generators examples/TriangleGraph) 'e23))
    (is (contains? (:generators examples/TriangleGraph) 'e31))

    ;; Vertices should have type V
    (is (= 'V (:type (get (:generators examples/TriangleGraph) 'v1))))

    ;; Edges should have type E
    (is (= 'E (:type (get (:generators examples/TriangleGraph) 'e12))))))

(deftest test-path-graph
  (testing "PathGraph presentation is valid"
    (is (pres/presentation? examples/PathGraph))
    (is (= stdlib/ThGraph (:theory examples/PathGraph)))

    ;; Should have 7 generators: 4 vertices + 3 edges
    (is (= 7 (count (:generators examples/PathGraph))))

    ;; Check for vertices
    (is (contains? (:generators examples/PathGraph) 'v1))
    (is (contains? (:generators examples/PathGraph) 'v2))
    (is (contains? (:generators examples/PathGraph) 'v3))
    (is (contains? (:generators examples/PathGraph) 'v4))

    ;; Check for edges
    (is (contains? (:generators examples/PathGraph) 'e1))
    (is (contains? (:generators examples/PathGraph) 'e2))
    (is (contains? (:generators examples/PathGraph) 'e3))))

(deftest test-star-graph
  (testing "StarGraph presentation is valid"
    (is (pres/presentation? examples/StarGraph))
    (is (= stdlib/ThGraph (:theory examples/StarGraph)))

    ;; Should have 9 generators: 5 vertices (1 center + 4 outer) + 4 edges
    (is (= 9 (count (:generators examples/StarGraph))))

    ;; Check for center vertex
    (is (contains? (:generators examples/StarGraph) 'center))

    ;; Check for outer vertices
    (is (contains? (:generators examples/StarGraph) 'v1))
    (is (contains? (:generators examples/StarGraph) 'v2))
    (is (contains? (:generators examples/StarGraph) 'v3))
    (is (contains? (:generators examples/StarGraph) 'v4))))

(deftest test-finite-category
  (testing "FiniteCategory presentation is valid"
    (is (pres/presentation? examples/FiniteCategory))
    (is (= stdlib/ThCategory (:theory examples/FiniteCategory)))

    ;; Should have 6 generators: 3 objects + 3 morphisms
    (is (= 6 (count (:generators examples/FiniteCategory))))

    ;; Check for objects
    (is (contains? (:generators examples/FiniteCategory) 'a))
    (is (contains? (:generators examples/FiniteCategory) 'b))
    (is (contains? (:generators examples/FiniteCategory) 'c))
    (is (= 'Ob (:type (get (:generators examples/FiniteCategory) 'a))))

    ;; Check for morphisms
    (is (contains? (:generators examples/FiniteCategory) 'f))
    (is (contains? (:generators examples/FiniteCategory) 'g))
    (is (contains? (:generators examples/FiniteCategory) 'h))
    (is (= 'Hom (:type (get (:generators examples/FiniteCategory) 'f))))))

(deftest test-monoid-as-category
  (testing "MonoidAsCategory presentation is valid"
    (is (pres/presentation? examples/MonoidAsCategory))
    (is (= stdlib/ThCategory (:theory examples/MonoidAsCategory)))

    ;; Should have 4 generators: 1 object + 3 morphisms
    (is (= 4 (count (:generators examples/MonoidAsCategory))))

    ;; Check for single object
    (is (contains? (:generators examples/MonoidAsCategory) 'star))
    (is (= 'Ob (:type (get (:generators examples/MonoidAsCategory) 'star))))

    ;; Check for morphisms (monoid elements)
    (is (contains? (:generators examples/MonoidAsCategory) 'm1))
    (is (contains? (:generators examples/MonoidAsCategory) 'm2))
    (is (contains? (:generators examples/MonoidAsCategory) 'm3))))

(deftest test-free-monoid
  (testing "FreeMonoid presentation is valid"
    (is (pres/presentation? examples/FreeMonoid))
    (is (= stdlib/ThMonoid (:theory examples/FreeMonoid)))

    ;; Should have 3 generators: x, y, z
    (is (= 3 (count (:generators examples/FreeMonoid))))

    ;; Check for generators
    (is (contains? (:generators examples/FreeMonoid) 'x))
    (is (contains? (:generators examples/FreeMonoid) 'y))
    (is (contains? (:generators examples/FreeMonoid) 'z))

    ;; All should have type El
    (is (= 'El (:type (get (:generators examples/FreeMonoid) 'x))))
    (is (= 'El (:type (get (:generators examples/FreeMonoid) 'y))))
    (is (= 'El (:type (get (:generators examples/FreeMonoid) 'z))))))

(deftest test-natural-numbers-monoid
  (testing "NaturalNumbersMonoid presentation is valid"
    (is (pres/presentation? examples/NaturalNumbersMonoid))
    (is (= stdlib/ThMonoid (:theory examples/NaturalNumbersMonoid)))

    ;; Should have 4 generators: zero, one, two, three
    (is (= 4 (count (:generators examples/NaturalNumbersMonoid))))

    (is (contains? (:generators examples/NaturalNumbersMonoid) 'zero))
    (is (contains? (:generators examples/NaturalNumbersMonoid) 'one))
    (is (contains? (:generators examples/NaturalNumbersMonoid) 'two))
    (is (contains? (:generators examples/NaturalNumbersMonoid) 'three))))

(deftest test-cyclic-group3
  (testing "CyclicGroup3 presentation is valid"
    (is (pres/presentation? examples/CyclicGroup3))
    (is (= stdlib/ThGroup (:theory examples/CyclicGroup3)))

    ;; Should have 3 generators: e (identity), r (rotation), r2 (rotation^2)
    (is (= 3 (count (:generators examples/CyclicGroup3))))

    (is (contains? (:generators examples/CyclicGroup3) 'e))
    (is (contains? (:generators examples/CyclicGroup3) 'r))
    (is (contains? (:generators examples/CyclicGroup3) 'r2))

    ;; All should have type El
    (is (= 'El (:type (get (:generators examples/CyclicGroup3) 'e))))
    (is (= 'El (:type (get (:generators examples/CyclicGroup3) 'r))))))

(deftest test-klein4-group
  (testing "Klein4Group presentation is valid"
    (is (pres/presentation? examples/Klein4Group))
    (is (= stdlib/ThGroup (:theory examples/Klein4Group)))

    ;; Should have 4 generators: e, a, b, c
    (is (= 4 (count (:generators examples/Klein4Group))))

    (is (contains? (:generators examples/Klein4Group) 'e))
    (is (contains? (:generators examples/Klein4Group) 'a))
    (is (contains? (:generators examples/Klein4Group) 'b))
    (is (contains? (:generators examples/Klein4Group) 'c))))

;;; ============================================================================
;;; Example Utility Function Tests
;;; ============================================================================

(deftest test-list-examples
  (testing "list-examples returns all example categories"
    (let [examples-list (examples/list-examples)]
      (is (= 4 (count (keys examples-list))))
      (is (contains? examples-list :graphs))
      (is (contains? examples-list :categories))
      (is (contains? examples-list :monoids))
      (is (contains? examples-list :groups))

      ;; Check graph examples
      (is (= 3 (count (:graphs examples-list))))
      (is (some #(= 'TriangleGraph %) (:graphs examples-list)))
      (is (some #(= 'PathGraph %) (:graphs examples-list)))
      (is (some #(= 'StarGraph %) (:graphs examples-list)))

      ;; Check category examples
      (is (= 2 (count (:categories examples-list))))

      ;; Check monoid examples
      (is (= 2 (count (:monoids examples-list))))

      ;; Check group examples
      (is (= 2 (count (:groups examples-list)))))))

(deftest test-example-summary
  (testing "example-summary returns correct information for TriangleGraph"
    (let [summary (examples/example-summary #'examples/TriangleGraph)]
      (is (= 'TriangleGraph (:name summary)))
      (is (= 'ThGraph (:theory summary)))
      (is (= 6 (:generators summary)))
      (is (= 0 (:equations summary)))))

  (testing "example-summary returns correct information for FreeMonoid"
    (let [summary (examples/example-summary #'examples/FreeMonoid)]
      (is (= 'FreeMonoid (:name summary)))
      (is (= 'ThMonoid (:theory summary)))
      (is (= 3 (:generators summary))))))

;;; ============================================================================
;;; Cross-Theory Validation Tests
;;; ============================================================================

(deftest test-all-examples-match-their-theories
  (testing "All example presentations match their declared theory"
    (doseq [example ['TriangleGraph 'PathGraph 'StarGraph]]
      (let [pres (var-get (resolve (symbol "katzen.stdlib.examples" (name example))))]
        (is (= stdlib/ThGraph (:theory pres))
            (str example " should have ThGraph as theory"))))

    (doseq [example ['FiniteCategory 'MonoidAsCategory]]
      (let [pres (var-get (resolve (symbol "katzen.stdlib.examples" (name example))))]
        (is (= stdlib/ThCategory (:theory pres))
            (str example " should have ThCategory as theory"))))

    (doseq [example ['FreeMonoid 'NaturalNumbersMonoid]]
      (let [pres (var-get (resolve (symbol "katzen.stdlib.examples" (name example))))]
        (is (= stdlib/ThMonoid (:theory pres))
            (str example " should have ThMonoid as theory"))))

    (doseq [example ['CyclicGroup3 'Klein4Group]]
      (let [pres (var-get (resolve (symbol "katzen.stdlib.examples" (name example))))]
        (is (= stdlib/ThGroup (:theory pres))
            (str example " should have ThGroup as theory"))))))

(deftest test-all-examples-have-valid-generators
  (testing "All examples have at least one generator"
    (is (> (count (:generators examples/TriangleGraph)) 0))
    (is (> (count (:generators examples/PathGraph)) 0))
    (is (> (count (:generators examples/StarGraph)) 0))
    (is (> (count (:generators examples/FiniteCategory)) 0))
    (is (> (count (:generators examples/MonoidAsCategory)) 0))
    (is (> (count (:generators examples/FreeMonoid)) 0))
    (is (> (count (:generators examples/NaturalNumbersMonoid)) 0))
    (is (> (count (:generators examples/CyclicGroup3)) 0))
    (is (> (count (:generators examples/Klein4Group)) 0))))

(deftest test-all-theories-are-valid
  (testing "All standard theories pass validation"
    (is (core/gat? stdlib/ThGraph))
    (is (core/gat? stdlib/ThCategory))
    (is (core/gat? stdlib/ThSymmetricMonoidalCategory))
    (is (core/gat? stdlib/ThMonoid))
    (is (core/gat? stdlib/ThGroup))))
