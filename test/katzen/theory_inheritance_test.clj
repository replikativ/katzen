(ns katzen.theory-inheritance-test
  "Tests for `(using ThParent)` inheritance in deftheory.

   Mirrors GATlab.jl's mid-2024 PR #147 multi-inheritance (linear chains only
   for now; diamond resolution is a phase 2 concern)."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.theory :as theory]
            [katzen.core :as core]))

;;; ============================================================================
;;; Linear inheritance chain
;;;
;;; ThSet ⊂ ThPointedSet ⊂ ThPointedSetWithIdLaw
;;; ============================================================================

(theory/deftheory ThSet
  (type El))

(theory/deftheory ThPointedSet
  (using ThSet)
  (term pt :ret El))

(theory/deftheory ThPointedSetWithIdLaw
  (using ThPointedSet)
  (axiom id-pt :ctx [x El] (= x x)))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(deftest test-inheritance-preserves-parent-sorts
  (testing "Child theory has parent's type constructors"
    (is (= 1 (count (:type-constructors ThSet))))
    (is (= 1 (count (:type-constructors ThPointedSet))) "ThPointedSet inherits El")
    (is (= 'El (-> ThPointedSet :type-constructors first :type :head :name)))))

(deftest test-inheritance-preserves-parent-terms
  (testing "Child theory has parent's term constructors plus its own"
    (is (empty? (:term-constructors ThSet)))
    (is (= 1 (count (:term-constructors ThPointedSet))))
    (is (= 'pt (-> ThPointedSet :term-constructors first :term :head :name)))
    (is (= 1 (count (:term-constructors ThPointedSetWithIdLaw)))
        "ThPointedSetWithIdLaw inherits pt from ThPointedSet")))

(deftest test-multi-level-chain
  (testing "Three-level chain accumulates declarations"
    (is (= 1 (count (:type-constructors ThPointedSetWithIdLaw))) "El inherited")
    (is (= 1 (count (:term-constructors ThPointedSetWithIdLaw))) "pt inherited")
    (is (= 1 (count (:axioms ThPointedSetWithIdLaw))) "id-pt declared locally")))

(deftest test-source-decls-metadata
  (testing "Theories stash expanded source-decls for downstream inheritance"
    (is (some? (-> ThSet meta :source-decls)))
    (is (some? (-> ThPointedSet meta :source-decls)))
    ;; ThPointedSet's expanded decls should contain ThSet's (type El) AND its own (term pt).
    (let [pset-decls (-> ThPointedSet meta :source-decls)]
      (is (some (fn [d] (and (= 'type (first d)) (= 'El (second d)))) pset-decls)
          "El sort visible in expanded source-decls")
      (is (some (fn [d] (and (= 'term (first d)) (= 'pt (second d)))) pset-decls)
          "pt term visible in expanded source-decls"))))

(deftest test-unknown-parent-throws
  (testing "Using an undefined parent fails with a clear message"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Parent theory not found"
         (theory/expand-using '[(using NoSuchTheory)])))))

(deftest test-duplicate-name-throws
  (testing "Two `using` clauses declaring the same name throw"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Duplicate declaration name"
         (theory/expand-using '[(using katzen.theory-inheritance-test/ThSet)
                                (using katzen.theory-inheritance-test/ThSet)])))))

(deftest test-no-using-still-works
  (testing "Existing deftheory without (using ...) is unaffected"
    (let [t-ctors (:type-constructors ThSet)]
      (is (= 1 (count t-ctors)))
      (is (= 'El (-> t-ctors first :type :head :name))))))
