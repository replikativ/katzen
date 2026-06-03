(ns katzen.morphism-wellformedness-test
  "Tests for theory-morphism wellformedness checks (migrated from the
   former katzen.validation namespace). The static type checker that
   formerly lived alongside these was removed when modern GATlab.jl
   removed its own (commit a1c16f8, Feb 2025)."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.theory :as theory]
            [katzen.morphism :as morph]
            [katzen.core :as core]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; Setup: Define theories for testing
;;; ============================================================================

(theory/deftheory ValidationCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))
  (term id
    :ctx [a Ob]
    :ret (Hom a a)))

(theory/deftheory Preorder
  (type default)
  (type Leq [dom default, codom default])
  (term trans
    :ctx [a default, b default, c default]
    :args [f (Leq a b), g (Leq b c)]
    :ret (Leq a c))
  (term refl
    :ctx [a default]
    :ret (Leq a a)))

;;; ============================================================================
;;; Morphism Validation Tests
;;; ============================================================================

(deftest test-valid-morphism
  (testing "Valid morphism passes validation"
    (let [cat-ob (first (:type-constructors ValidationCategory))
          cat-hom (second (:type-constructors ValidationCategory))
          cat-compose (first (:term-constructors ValidationCategory))
          cat-id (second (:term-constructors ValidationCategory))
          pre-default (first (:type-constructors Preorder))
          pre-leq (second (:type-constructors Preorder))
          pre-trans (first (:term-constructors Preorder))
          pre-refl (second (:term-constructors Preorder))
          type-map {(-> cat-ob :type :head) pre-default
                    (-> cat-hom :type :head) pre-leq}
          term-map {(-> cat-compose :term :head) pre-trans
                    (-> cat-id :term :head) pre-refl}
          morphism (morph/theory-morphism 'PreorderCat
                                          ValidationCategory
                                          Preorder
                                          type-map
                                          term-map)]
      (is (= morphism (morph/validate-theory-morphism morphism))))))

(deftest test-morphism-wrong-context-length
  (testing "Morphism with wrong context length fails"
    (let [tag (scope/scope-tag)
          wrong-ctx (core/type-ctx [(scope/ident tag 0 'a)
                                    (scope/ident tag 1 'b)
                                    (scope/ident tag 2 'c)
                                    (scope/ident tag 3 'd)]
                                   (repeat 4 (core/alg-type (scope/ident tag 10 'Ob) [] core/TYPE)))
          cat-compose (first (:term-constructors ValidationCategory))
          pre-trans (first (:term-constructors Preorder))
          wrong-trans-tic (core/term-in-ctx wrong-ctx (:term pre-trans))
          term-map {(-> cat-compose :term :head) wrong-trans-tic}
          morphism (morph/theory-morphism 'BadMorph
                                          ValidationCategory
                                          Preorder
                                          {}
                                          term-map)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"mismatched context lengths"
           (morph/validate-theory-morphism morphism))))))

(deftest test-morphism-bad-type-map
  (testing "Morphism with unmapped type fails"
    (let [cat-hom (second (:type-constructors ValidationCategory))
          tag (scope/scope-tag)
          fake-type (core/alg-type (scope/ident tag 0 'Nonexistent) [] core/TYPE)
          fake-tic (core/type-in-ctx (core/type-ctx) fake-type)
          type-map {(-> cat-hom :type :head) fake-tic}
          morphism (morph/theory-morphism 'BadMorph
                                          ValidationCategory
                                          Preorder
                                          type-map
                                          {})]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Type not found in codomain"
           (morph/validate-theory-morphism morphism))))))

(deftest test-morphism-mismatched-codom-context
  (testing "Morphism with mismatched codomain context fails"
    (let [cat-id (second (:term-constructors ValidationCategory))
          tag (scope/scope-tag)
          wrong-ctx (core/type-ctx [(scope/ident tag 0 'a)
                                    (scope/ident tag 1 'b)]
                                   (repeat 2 (core/alg-type (scope/ident tag 10 'default) [] core/TYPE)))
          pre-refl (second (:term-constructors Preorder))
          wrong-refl-tic (core/term-in-ctx wrong-ctx (:term pre-refl))
          term-map {(-> cat-id :term :head) wrong-refl-tic}
          morphism (morph/theory-morphism 'BadMorph
                                          ValidationCategory
                                          Preorder
                                          {}
                                          term-map)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"mismatched context lengths"
           (morph/validate-theory-morphism morphism))))))

(deftest test-type-mapping-validation
  (testing "validate-type-mapping with valid mapping"
    (let [cat-ob (first (:type-constructors ValidationCategory))
          pre-default (first (:type-constructors Preorder))
          dom-ident (-> cat-ob :type :head)
          codom-tic pre-default]
      (is (nil? (morph/validate-type-mapping ValidationCategory Preorder dom-ident codom-tic)))))
  (testing "validate-type-mapping with missing domain type"
    (let [tag (scope/scope-tag)
          fake-ident (scope/ident tag 0 'Nonexistent)
          pre-default (first (:type-constructors Preorder))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Type not found in domain"
           (morph/validate-type-mapping ValidationCategory Preorder fake-ident pre-default)))))
  (testing "validate-type-mapping with missing codomain type"
    (let [cat-ob (first (:type-constructors ValidationCategory))
          dom-ident (-> cat-ob :type :head)
          tag (scope/scope-tag)
          fake-type (core/alg-type (scope/ident tag 0 'Nonexistent) [] core/TYPE)
          fake-tic (core/type-in-ctx (core/type-ctx) fake-type)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Type not found in codomain"
           (morph/validate-type-mapping ValidationCategory Preorder dom-ident fake-tic))))))

(deftest test-term-mapping-validation
  (testing "validate-term-mapping with valid mapping"
    (let [cat-id (second (:term-constructors ValidationCategory))
          pre-refl (second (:term-constructors Preorder))
          dom-ident (-> cat-id :term :head)
          codom-tic pre-refl]
      (is (nil? (morph/validate-term-mapping ValidationCategory Preorder dom-ident codom-tic)))))
  (testing "validate-term-mapping with missing domain term"
    (let [tag (scope/scope-tag)
          fake-ident (scope/ident tag 0 'Nonexistent)
          pre-refl (second (:term-constructors Preorder))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Term not found in domain"
           (morph/validate-term-mapping ValidationCategory Preorder fake-ident pre-refl)))))
  (testing "validate-term-mapping with missing codomain term"
    (let [cat-id (second (:term-constructors ValidationCategory))
          dom-ident (-> cat-id :term :head)
          tag (scope/scope-tag)
          fake-term (core/alg-term (scope/ident tag 0 'Nonexistent)
                                   []
                                   (core/alg-type (scope/ident tag 1 'T) [] core/TYPE))
          fake-tic (core/term-in-ctx (core/type-ctx) fake-term)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Term not found in codomain"
           (morph/validate-term-mapping ValidationCategory Preorder dom-ident fake-tic))))))
