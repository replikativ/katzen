(ns katzen.morphism-test
  "Tests for theory morphisms."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.morphism :as morph]
            [katzen.theory :as theory]
            [katzen.core :as core]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; Setup: Define some theories to work with
;;; ============================================================================

(theory/deftheory SimpleTheory
  (type A)
  (type B))

(theory/deftheory AnotherTheory
  (type X)
  (type Y))

;;; ============================================================================
;;; Basic Theory Morphism Tests
;;; ============================================================================

(deftest test-theory-morphism-creation
  (testing "Can create a theory morphism"
    (let [morph (morph/theory-morphism 'TestMorph
                                       SimpleTheory
                                       AnotherTheory
                                       {}
                                       {})]
      (is (morph/theory-morphism? morph))
      (is (= 'TestMorph (:name morph)))
      (is (= SimpleTheory (:dom morph)))
      (is (= AnotherTheory (:codom morph))))))

;;; ============================================================================
;;; Substitution Tests
;;; ============================================================================

(deftest test-substitute-ident
  (testing "Can substitute idents"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          y (scope/ident tag 1 'y)
          ident-map {x y}]
      (is (= y (morph/substitute-ident ident-map x)))
      (is (= x (morph/substitute-ident {} x))
          "Unchanged if not in map"))))

(deftest test-substitute-in-type
  (testing "Can substitute in nullary type"
    (let [tag (scope/scope-tag)
          a-ident (scope/ident tag 0 'A)
          x-ident (scope/ident tag 1 'X)
          a-type (core/alg-type a-ident [] core/TYPE)
          x-type (core/alg-type x-ident [] core/TYPE)
          ident-map {a-ident x-ident}
          result (morph/substitute-in-type ident-map a-type)]
      (is (= x-ident (:head result)))))

  (testing "Can substitute in type with arguments"
    (let [tag (scope/scope-tag)
          hom-ident (scope/ident tag 0 'Hom)
          a (scope/ident tag 1 'a)
          b (scope/ident tag 2 'b)
          c (scope/ident tag 3 'c)
          hom-type (core/alg-type hom-ident [a b] core/TYPE)
          ident-map {a c}
          result (morph/substitute-in-type ident-map hom-type)]
      (is (= hom-ident (:head result)))
      (is (= c (first (:args result))))
      (is (= b (second (:args result)))))))

(deftest test-substitute-in-term
  (testing "Can substitute in simple term"
    (let [tag (scope/scope-tag)
          f-ident (scope/ident tag 0 'f)
          g-ident (scope/ident tag 1 'g)
          type (core/alg-type (scope/ident tag 2 'T) [] core/TYPE)
          f-term (core/alg-term f-ident [] type)
          g-term (core/alg-term g-ident [] type)
          ident-map {f-ident g-ident}
          result (morph/substitute-in-term ident-map f-term)]
      (is (= g-ident (:head result)))))

  (testing "Can substitute in term with arguments"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          f (scope/ident tag 1 'f)
          g (scope/ident tag 2 'g)
          h (scope/ident tag 3 'h)
          type (core/alg-type (scope/ident tag 4 'Hom) [] core/TYPE)
          term (core/alg-term compose [f g] type)
          ident-map {f h}
          result (morph/substitute-in-term ident-map term)]
      (is (= compose (:head result)))
      (is (= h (first (:args result))))
      (is (= g (second (:args result))))))

  (testing "Can substitute in nested term"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          f (scope/ident tag 1 'f)
          g (scope/ident tag 2 'g)
          h (scope/ident tag 3 'h)
          type (core/alg-type (scope/ident tag 4 'Hom) [] core/TYPE)
          inner (core/alg-term compose [f g] type)
          outer (core/alg-term compose [inner h] type)
          ident-map {g h}
          result (morph/substitute-in-term ident-map outer)]
      ;; The inner term should have g->h substituted
      (is (core/alg-term? (first (:args result))))
      (is (= h (second (:args (first (:args result)))))))))

;;; ============================================================================
;;; Pushforward Tests
;;; ============================================================================

(deftest test-build-ident-map
  (testing "Can build ident map from context"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          ;; Domain context
          a (scope/ident tag1 0 'a)
          b (scope/ident tag1 1 'b)
          ob-type (core/alg-type (scope/ident tag1 2 'Ob) [] core/TYPE)
          ctx (core/type-ctx [a b] [ob-type ob-type])

          ;; Codomain mappings
          x (scope/ident tag2 0 'x)
          y (scope/ident tag2 1 'y)
          x-type (core/alg-type x [] core/TYPE)
          y-type (core/alg-type y [] core/TYPE)

          type-map {a (core/type-in-ctx (core/type-ctx) x-type)
                    b (core/type-in-ctx (core/type-ctx) y-type)}

          ident-map (morph/build-ident-map type-map ctx)]

      (is (= x-type (get ident-map a)))
      (is (= y-type (get ident-map b))))))

(deftest test-pushforward-type
  (testing "Can pushforward a simple type"
    (let [;; Create simple morphism A->X, B->Y
          tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)

          a-ident (scope/ident tag1 0 'A)
          a-type (core/alg-type a-ident [] core/TYPE)
          a-tic (core/type-in-ctx (core/type-ctx) a-type)

          x-ident (scope/ident tag2 0 'X)
          x-type (core/alg-type x-ident [] core/TYPE)
          x-tic (core/type-in-ctx (core/type-ctx) x-type)

          type-map {a-ident x-tic}
          morph (morph/theory-morphism 'TestMorph
                                       SimpleTheory
                                       AnotherTheory
                                       type-map
                                       {})

          result (morph/pushforward-type morph a-tic)]

      (is (core/type-in-ctx? result))
      (is (= x-ident (-> result :type :head))))))

(deftest test-pushforward-term
  (testing "Can pushforward a simple term"
    (let [;; Create simple morphism with term mapping
          tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)

          f-ident (scope/ident tag1 0 'f)
          type1 (core/alg-type (scope/ident tag1 1 'T) [] core/TYPE)
          f-term (core/alg-term f-ident [] type1)
          f-tic (core/term-in-ctx (core/type-ctx) f-term)

          g-ident (scope/ident tag2 0 'g)
          type2 (core/alg-type (scope/ident tag2 1 'T) [] core/TYPE)
          g-term (core/alg-term g-ident [] type2)
          g-tic (core/term-in-ctx (core/type-ctx) g-term)

          term-map {f-ident g-tic}
          morph (morph/theory-morphism 'TestMorph
                                       SimpleTheory
                                       AnotherTheory
                                       {}
                                       term-map)

          result (morph/pushforward-term morph f-tic)]

      (is (core/term-in-ctx? result))
      (is (= g-ident (-> result :term :head))))))

;;; ============================================================================
;;; Integration Test: Category to Opposite Category
;;; ============================================================================

(theory/deftheory MiniCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob]))

(deftest test-opposite-category-morphism
  (testing "Can create identity morphism on MiniCategory"
    ;; This is a simplified test - full opposite would swap Hom arguments
    (let [;; Get the type constructors
          ob-tic (first (:type-constructors MiniCategory))
          hom-tic (second (:type-constructors MiniCategory))

          ;; Create identity mapping (simplified)
          type-map {(-> ob-tic :type :head) ob-tic
                    (-> hom-tic :type :head) hom-tic}

          morph (morph/theory-morphism 'OpCat
                                       MiniCategory
                                       MiniCategory
                                       type-map
                                       {})]

      (is (morph/theory-morphism? morph))
      (is (= 'OpCat (:name morph)))
      (is (= MiniCategory (:dom morph)))
      (is (= MiniCategory (:codom morph))))))

;;; ============================================================================
;;; Pretty Printing Tests
;;; ============================================================================

(deftest test-format-morphism
  (testing "Can format a morphism"
    (let [morph (morph/theory-morphism 'TestMorph
                                       SimpleTheory
                                       AnotherTheory
                                       {}
                                       {})
          output (morph/format-morphism morph)]
      (is (string? output))
      (is (re-find #"Morphism: TestMorph" output))
      (is (re-find #"SimpleTheory => AnotherTheory" output)))))
