(ns katzen.expr-interop-test
  "Tests for expression interoperability."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.expr-interop :as interop]
            [katzen.core :as core]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; toexpr Tests
;;; ============================================================================

(deftest test-ident-toexpr-simple
  (testing "Simple ident converts to symbol"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)]
      (is (= 'x (interop/ident-toexpr x))))))

(deftest test-ident-toexpr-anonymous
  (testing "Anonymous ident converts to var\"#N\""
    (let [tag (scope/scope-tag)
          anon (scope/ident tag 5 nil)
          result (interop/ident-toexpr anon)]
      (is (symbol? result))
      (is (= "var\"#5\"" (str result))))))

(deftest test-type-toexpr-nullary
  (testing "Nullary type converts to simple symbol"
    (let [tag (scope/scope-tag)
          ob-ident (scope/ident tag 0 'Ob)
          ob-type (core/alg-type ob-ident [] core/TYPE)]
      (is (= 'Ob (interop/type-toexpr ob-type))))))

(deftest test-type-toexpr-with-args
  (testing "Type with arguments converts to list"
    (let [tag (scope/scope-tag)
          hom-ident (scope/ident tag 0 'Hom)
          a (scope/ident tag 1 'a)
          b (scope/ident tag 2 'b)
          hom-type (core/alg-type hom-ident [a b] core/TYPE)]
      (is (= '(Hom a b) (interop/type-toexpr hom-type))))))

(deftest test-term-toexpr-simple
  (testing "Simple term converts to symbol"
    (let [tag (scope/scope-tag)
          f (scope/ident tag 0 'f)
          type (core/alg-type (scope/ident tag 1 'T) [] core/TYPE)
          f-term (core/alg-term f [] type)]
      (is (= 'f (interop/term-toexpr f-term))))))

(deftest test-term-toexpr-with-args
  (testing "Term with arguments converts to list"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          f (scope/ident tag 1 'f)
          g (scope/ident tag 2 'g)
          type (core/alg-type (scope/ident tag 3 'Hom) [] core/TYPE)
          term (core/alg-term compose [f g] type)]
      (is (= '(compose f g) (interop/term-toexpr term))))))

(deftest test-term-toexpr-nested
  (testing "Nested term converts correctly"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          f (scope/ident tag 1 'f)
          g (scope/ident tag 2 'g)
          h (scope/ident tag 3 'h)
          type (core/alg-type (scope/ident tag 4 'Hom) [] core/TYPE)
          inner (core/alg-term compose [f g] type)
          outer (core/alg-term compose [inner h] type)]
      (is (= '(compose (compose f g) h) (interop/term-toexpr outer))))))

(deftest test-toexpr-dispatch
  (testing "toexpr dispatches correctly on type"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          ob-type (core/alg-type (scope/ident tag 1 'Ob) [] core/TYPE)
          f-term (core/alg-term (scope/ident tag 2 'f) [] ob-type)
          sort (core/alg-sort 'TYPE)]

      (is (= 'x (interop/toexpr x)))
      (is (= 'Ob (interop/toexpr ob-type)))
      (is (= 'f (interop/toexpr f-term)))
      (is (= 'TYPE (interop/toexpr sort))))))

;;; ============================================================================
;;; Multi-level Scope Tests
;;; ============================================================================

(deftest test-scope-list-creation
  (testing "Can create a ScopeList"
    (let [scope1 (scope/scope-context (scope/scope-tag))
          scope2 (scope/scope-context (scope/scope-tag))
          slist (interop/scope-list [scope1 scope2])]
      (is (interop/scope-list? slist))
      (is (= 2 (count (:levels slist)))))))

(deftest test-ident-disambiguation
  (testing "Idents from different scopes are disambiguated"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          scope1 (scope/scope-context tag1)
          scope2 (scope/scope-context tag2)
          [scope1 x1] (scope/bind scope1 'x)
          [scope2 x2] (scope/bind scope2 'x)

          ;; Most recent scope first
          slist (interop/scope-list [scope2 scope1])]

      ;; x from most recent scope (scope2) has no suffix
      (is (= 'x (interop/ident-toexpr slist x2)))
      ;; x from older scope (scope1) has !2 suffix
      (is (= 'x!2 (interop/ident-toexpr slist x1))))))

(deftest test-anonymous-ident-disambiguation
  (testing "Anonymous idents are disambiguated with levels"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          scope1 (scope/scope-context tag1)
          scope2 (scope/scope-context tag2)

          anon1 (scope/ident tag1 0 nil)
          anon2 (scope/ident tag2 0 nil)

          slist (interop/scope-list [scope2 scope1])
          result1 (interop/ident-toexpr slist anon2)
          result2 (interop/ident-toexpr slist anon1)]

      (is (= "var\"#0\"" (str result1)))
      (is (= "var\"#0!2\"" (str result2))))))

;;; ============================================================================
;;; fromexpr Tests
;;; ============================================================================

(deftest test-fromexpr-ident
  (testing "Symbol converts to Ident"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx x] (scope/bind scope-ctx 'x)
          result (interop/fromexpr-ident scope-ctx 'x)]
      (is (scope/gat-ident? result))
      (is (= x result)))))

(deftest test-fromexpr-type-nullary
  (testing "Simple symbol converts to nullary type"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx ob-ident] (scope/bind scope-ctx 'Ob)
          result (interop/fromexpr-type scope-ctx 'Ob)]
      (is (core/alg-type? result))
      (is (= ob-ident (:head result)))
      (is (empty? (:args result))))))

(deftest test-fromexpr-type-with-args
  (testing "List converts to type with arguments"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx hom-ident] (scope/bind scope-ctx 'Hom)
          [scope-ctx a] (scope/bind scope-ctx 'a)
          [scope-ctx b] (scope/bind scope-ctx 'b)
          result (interop/fromexpr-type scope-ctx '(Hom a b))]
      (is (core/alg-type? result))
      (is (= hom-ident (:head result)))
      (is (= [a b] (:args result))))))

(deftest test-fromexpr-term
  (testing "Symbol converts to simple term"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx f] (scope/bind scope-ctx 'f)
          type (core/alg-type (scope/ident tag 10 'T) [] core/TYPE)
          result (interop/fromexpr-term scope-ctx 'f type)]
      (is (core/alg-term? result))
      (is (= f (:head result)))
      (is (empty? (:args result))))))

(deftest test-fromexpr-term-with-args
  (testing "List converts to term with arguments"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx compose] (scope/bind scope-ctx 'compose)
          [scope-ctx f] (scope/bind scope-ctx 'f)
          [scope-ctx g] (scope/bind scope-ctx 'g)
          type (core/alg-type (scope/ident tag 10 'Hom) [] core/TYPE)
          result (interop/fromexpr-term scope-ctx '(compose f g) type)]
      (is (core/alg-term? result))
      (is (= compose (:head result)))
      (is (= [f g] (:args result))))))

(deftest test-fromexpr-term-nested
  (testing "Nested list converts to nested term"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx compose] (scope/bind scope-ctx 'compose)
          [scope-ctx f] (scope/bind scope-ctx 'f)
          [scope-ctx g] (scope/bind scope-ctx 'g)
          [scope-ctx h] (scope/bind scope-ctx 'h)
          type (core/alg-type (scope/ident tag 10 'Hom) [] core/TYPE)
          result (interop/fromexpr-term scope-ctx '(compose (compose f g) h) type)]
      (is (core/alg-term? result))
      (is (= compose (:head result)))
      ;; First arg is a nested term
      (is (core/alg-term? (first (:args result))))
      (is (= compose (:head (first (:args result)))))
      ;; Second arg is an ident
      (is (= h (second (:args result)))))))

;;; ============================================================================
;;; Round-trip Tests
;;; ============================================================================

(deftest test-roundtrip-type
  (testing "Type round-trips through toexpr/fromexpr"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx hom] (scope/bind scope-ctx 'Hom)
          [scope-ctx a] (scope/bind scope-ctx 'a)
          [scope-ctx b] (scope/bind scope-ctx 'b)

          original (core/alg-type hom [a b] core/TYPE)
          expr (interop/type-toexpr original)
          reconstructed (interop/fromexpr-type scope-ctx expr)]

      (is (= original reconstructed)))))

(deftest test-roundtrip-term
  (testing "Term round-trips through toexpr/fromexpr"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx compose] (scope/bind scope-ctx 'compose)
          [scope-ctx f] (scope/bind scope-ctx 'f)
          [scope-ctx g] (scope/bind scope-ctx 'g)

          type (core/alg-type (scope/ident tag 10 'Hom) [] core/TYPE)
          original (core/alg-term compose [f g] type)
          expr (interop/term-toexpr original)
          reconstructed (interop/fromexpr-term scope-ctx expr type)]

      (is (= original reconstructed)))))

(deftest test-roundtrip-nested-term
  (testing "Nested term round-trips correctly"
    (let [tag (scope/scope-tag)
          scope-ctx (scope/scope-context tag)
          [scope-ctx compose] (scope/bind scope-ctx 'compose)
          [scope-ctx f] (scope/bind scope-ctx 'f)
          [scope-ctx g] (scope/bind scope-ctx 'g)
          [scope-ctx h] (scope/bind scope-ctx 'h)

          type (core/alg-type (scope/ident tag 10 'Hom) [] core/TYPE)
          inner (core/alg-term compose [f g] type)
          original (core/alg-term compose [inner h] type)
          expr (interop/term-toexpr original)
          reconstructed (interop/fromexpr-term scope-ctx expr type)]

      (is (= original reconstructed)))))

;;; ============================================================================
;;; Parse Helper Tests
;;; ============================================================================

(deftest test-parse-disambiguated-symbol
  (testing "Parse symbol without suffix"
    (is (= ['x 1] (interop/parse-disambiguated-symbol 'x))))

  (testing "Parse symbol with level suffix"
    (is (= ['x 2] (interop/parse-disambiguated-symbol 'x!2)))))

(deftest test-parse-anonymous-symbol
  (testing "Parse anonymous symbol without suffix"
    (is (= [5 1] (interop/parse-anonymous-symbol (symbol "var\"#5\"")))))

  (testing "Parse anonymous symbol with level suffix"
    (is (= [5 2] (interop/parse-anonymous-symbol (symbol "var\"#5!2\"")))))

  (testing "Parse returns nil for non-anonymous symbol"
    (is (nil? (interop/parse-anonymous-symbol 'x)))))
