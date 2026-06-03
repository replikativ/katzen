(ns katzen.core-test
  "Comprehensive tests for core GAT data structures."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.core :as core]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; AlgSort Tests
;;; ============================================================================

(deftest test-alg-sort-creation
  (testing "Can create algebraic sorts"
    (let [sort (core/alg-sort 'TYPE)]
      (is (core/alg-sort? sort))
      (is (= 'TYPE (:name sort)))))

  (testing "TYPE constant exists"
    (is (core/alg-sort? core/TYPE))
    (is (= 'TYPE (:name core/TYPE)))))

;;; ============================================================================
;;; AlgType Tests
;;; ============================================================================

(deftest test-alg-type-creation
  (testing "Can create nullary type (type constant)"
    (let [tag (scope/scope-tag)
          ob (scope/ident tag 0 'Ob)
          type (core/alg-type ob [] core/TYPE)]
      (is (core/alg-type? type))
      (is (= ob (:head type)))
      (is (empty? (:args type)))
      (is (= core/TYPE (:sort type)))))

  (testing "Can create type with arguments"
    (let [tag (scope/scope-tag)
          hom (scope/ident tag 0 'Hom)
          ob1 (scope/ident tag 1 'a)
          ob2 (scope/ident tag 2 'b)
          type (core/alg-type hom [ob1 ob2] core/TYPE)]
      (is (= 2 (count (:args type))))
      (is (= [ob1 ob2] (:args type)))))

  (testing "String representation works"
    (let [tag (scope/scope-tag)
          ob (scope/ident tag 0 'Ob)
          type (core/alg-type ob [] core/TYPE)]
      (is (string? (str type)))
      (is (re-find #"Ident" (str type))))))

;;; ============================================================================
;;; AlgTerm Tests
;;; ============================================================================

(deftest test-alg-term-creation
  (testing "Can create terms"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          term (core/alg-term compose [] hom-type)]
      (is (core/alg-term? term))
      (is (= compose (:head term)))
      (is (= hom-type (:type term)))))

  (testing "Terms can have arguments"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          f (scope/ident tag 1 'f)
          g (scope/ident tag 2 'g)
          hom-type (core/alg-type (scope/ident tag 3 'Hom) [] core/TYPE)
          term (core/alg-term compose [f g] hom-type)]
      (is (= 2 (count (:args term))))
      (is (= [f g] (:args term))))))

;;; ============================================================================
;;; TypeCtx Tests
;;; ============================================================================

(deftest test-type-ctx-creation
  (testing "Can create empty type context"
    (let [ctx (core/type-ctx)]
      (is (core/type-ctx? ctx))
      (is (empty? (:idents ctx)))
      (is (empty? (:types ctx)))))

  (testing "Can create context with bindings"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          ob-type (core/alg-type (scope/ident tag 1 'Ob) [] core/TYPE)
          ctx (core/type-ctx [x] [ob-type])]
      (is (= 1 (core/context-length ctx)))
      (is (= [x] (:idents ctx)))
      (is (= [ob-type] (:types ctx)))))

  (testing "Construction validates parallel vectors"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          y (scope/ident tag 1 'y)
          ob-type (core/alg-type (scope/ident tag 2 'Ob) [] core/TYPE)]
      (is (thrown? AssertionError
                   (core/type-ctx [x y] [ob-type]))
          "Should fail when counts don't match"))))

(deftest test-add-binding
  (testing "Can add single binding to context"
    (let [tag (scope/scope-tag)
          ctx (core/type-ctx)
          x (scope/ident tag 0 'x)
          ob-type (core/alg-type (scope/ident tag 1 'Ob) [] core/TYPE)
          ctx2 (core/add-binding ctx x ob-type)]
      (is (= 1 (core/context-length ctx2)))
      (is (= x (first (:idents ctx2))))
      (is (= ob-type (first (:types ctx2))))))

  (testing "Can add multiple bindings sequentially"
    (let [tag (scope/scope-tag)
          ctx (core/type-ctx)
          x (scope/ident tag 0 'x)
          y (scope/ident tag 1 'y)
          ob-type (core/alg-type (scope/ident tag 2 'Ob) [] core/TYPE)
          ctx2 (-> ctx
                   (core/add-binding x ob-type)
                   (core/add-binding y ob-type))]
      (is (= 2 (core/context-length ctx2)))
      (is (= [x y] (:idents ctx2))))))

(deftest test-add-bindings
  (testing "Can add multiple bindings at once"
    (let [tag (scope/scope-tag)
          ctx (core/type-ctx)
          x (scope/ident tag 0 'x)
          y (scope/ident tag 1 'y)
          ob-type (core/alg-type (scope/ident tag 2 'Ob) [] core/TYPE)
          ctx2 (core/add-bindings ctx [x y] [ob-type ob-type])]
      (is (= 2 (core/context-length ctx2)))
      (is (= [x y] (:idents ctx2))))))

(deftest test-lookup-type
  (testing "Can lookup type of ident in context"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          ob-type (core/alg-type (scope/ident tag 1 'Ob) [] core/TYPE)
          ctx (core/type-ctx [x] [ob-type])]
      (is (= ob-type (core/lookup-type ctx x)))))

  (testing "Returns nil for unbound ident"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          y (scope/ident tag 1 'y)
          ob-type (core/alg-type (scope/ident tag 2 'Ob) [] core/TYPE)
          ctx (core/type-ctx [x] [ob-type])]
      (is (nil? (core/lookup-type ctx y)))))

  (testing "has-binding-for? works correctly"
    (let [tag (scope/scope-tag)
          x (scope/ident tag 0 'x)
          y (scope/ident tag 1 'y)
          ob-type (core/alg-type (scope/ident tag 2 'Ob) [] core/TYPE)
          ctx (core/type-ctx [x] [ob-type])]
      (is (core/has-binding-for? ctx x))
      (is (not (core/has-binding-for? ctx y))))))

;;; ============================================================================
;;; TypeInCtx and TermInCtx Tests
;;; ============================================================================

(deftest test-type-in-ctx
  (testing "Can create type in context"
    (let [tag (scope/scope-tag)
          ctx (core/type-ctx)
          ob-type (core/alg-type (scope/ident tag 0 'Ob) [] core/TYPE)
          tic (core/type-in-ctx ctx ob-type)]
      (is (core/type-in-ctx? tic))
      (is (= ctx (:ctx tic)))
      (is (= ob-type (:type tic))))))

(deftest test-term-in-ctx
  (testing "Can create term in context"
    (let [tag (scope/scope-tag)
          ctx (core/type-ctx)
          id (scope/ident tag 0 'id)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          term (core/alg-term id [] hom-type)
          tic (core/term-in-ctx ctx term)]
      (is (core/term-in-ctx? tic))
      (is (= ctx (:ctx tic)))
      (is (= term (:term tic))))))

;;; ============================================================================
;;; AlgAxiom Tests
;;; ============================================================================

(deftest test-alg-axiom-creation
  (testing "Can create axiom"
    (let [tag (scope/scope-tag)
          ctx (core/type-ctx)
          compose (scope/ident tag 0 'compose)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          lhs (core/alg-term compose [] hom-type)
          rhs (core/alg-term compose [] hom-type)
          axiom (core/alg-axiom 'test-axiom ctx lhs rhs)]
      (is (core/alg-axiom? axiom))
      (is (= 'test-axiom (:name axiom)))
      (is (= ctx (:ctx axiom)))
      (is (= lhs (:lhs axiom)))
      (is (= rhs (:rhs axiom))))))

;;; ============================================================================
;;; GAT Tests
;;; ============================================================================

(deftest test-gat-creation
  (testing "Can create empty GAT"
    (let [gat (core/empty-gat 'TestTheory)]
      (is (core/gat? gat))
      (is (= 'TestTheory (:name gat)))
      (is (scope/scope-tag? (:tag gat)))
      (is (= [core/TYPE] (:sorts gat)))
      (is (empty? (:type-constructors gat)))
      (is (empty? (:term-constructors gat)))
      (is (empty? (:axioms gat)))))

  (testing "Can create GAT with components"
    (let [tag (scope/scope-tag)
          ob-ident (scope/ident tag 0 'Ob)
          ob-type (core/alg-type ob-ident [] core/TYPE)
          tic (core/type-in-ctx (core/type-ctx) ob-type)
          gat (core/gat 'TestTheory tag [core/TYPE] [tic] [] [])]
      (is (= 1 (count (:type-constructors gat))))
      (is (= tic (first (:type-constructors gat)))))))

(deftest test-gat-add-type-constructor
  (testing "Can add type constructor to GAT"
    (let [gat (core/empty-gat 'TestTheory)
          tag (:tag gat)
          ob (scope/ident tag 0 'Ob)
          ob-type (core/alg-type ob [] core/TYPE)
          tic (core/type-in-ctx (core/type-ctx) ob-type)
          gat2 (core/add-type-constructor gat tic)]
      (is (= 1 (count (:type-constructors gat2))))
      (is (= tic (first (:type-constructors gat2)))))))

(deftest test-gat-add-term-constructor
  (testing "Can add term constructor to GAT"
    (let [gat (core/empty-gat 'TestTheory)
          tag (:tag gat)
          id (scope/ident tag 0 'id)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          term (core/alg-term id [] hom-type)
          tic (core/term-in-ctx (core/type-ctx) term)
          gat2 (core/add-term-constructor gat tic)]
      (is (= 1 (count (:term-constructors gat2))))
      (is (= tic (first (:term-constructors gat2)))))))

(deftest test-gat-add-axiom
  (testing "Can add axiom to GAT"
    (let [gat (core/empty-gat 'TestTheory)
          tag (:tag gat)
          compose (scope/ident tag 0 'compose)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          lhs (core/alg-term compose [] hom-type)
          rhs (core/alg-term compose [] hom-type)
          axiom (core/alg-axiom 'test-axiom (core/type-ctx) lhs rhs)
          gat2 (core/add-axiom gat axiom)]
      (is (= 1 (count (:axioms gat2))))
      (is (= axiom (first (:axioms gat2)))))))

(deftest test-gat-getters
  (testing "Can get type constructor by name"
    (let [tag (scope/scope-tag)
          ob (scope/ident tag 0 'Ob)
          ob-type (core/alg-type ob [] core/TYPE)
          tic (core/type-in-ctx (core/type-ctx) ob-type)
          gat (-> (core/empty-gat 'TestTheory)
                  (assoc :tag tag)
                  (core/add-type-constructor tic))
          found (core/get-type-constructor gat 'Ob)]
      (is (= tic found))))

  (testing "Can get term constructor by name"
    (let [tag (scope/scope-tag)
          id (scope/ident tag 0 'id)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          term (core/alg-term id [] hom-type)
          tic (core/term-in-ctx (core/type-ctx) term)
          gat (-> (core/empty-gat 'TestTheory)
                  (assoc :tag tag)
                  (core/add-term-constructor tic))
          found (core/get-term-constructor gat 'id)]
      (is (= tic found))))

  (testing "Can get axiom by name"
    (let [tag (scope/scope-tag)
          compose (scope/ident tag 0 'compose)
          hom-type (core/alg-type (scope/ident tag 1 'Hom) [] core/TYPE)
          lhs (core/alg-term compose [] hom-type)
          rhs (core/alg-term compose [] hom-type)
          axiom (core/alg-axiom 'assoc (core/type-ctx) lhs rhs)
          gat (-> (core/empty-gat 'TestTheory)
                  (core/add-axiom axiom))
          found (core/get-axiom gat 'assoc)]
      (is (= axiom found)))))

;;; ============================================================================
;;; IScoped Implementation Tests
;;; ============================================================================

(deftest test-alg-type-retag
  (testing "AlgType retag works"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          hom (scope/ident tag1 0 'Hom)
          a (scope/ident tag1 1 'a)
          b (scope/ident tag1 2 'b)
          type (core/alg-type hom [a b] core/TYPE)
          retagged (scope/retag type {tag1 tag2})]
      (is (= tag2 (-> retagged :head :tag)))
      (is (= tag2 (-> retagged :args first :tag)))
      (is (= tag2 (-> retagged :args second :tag))))))

(deftest test-alg-term-retag
  (testing "AlgTerm retag works"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          compose (scope/ident tag1 0 'compose)
          f (scope/ident tag1 1 'f)
          g (scope/ident tag1 2 'g)
          hom (scope/ident tag1 3 'Hom)
          hom-type (core/alg-type hom [] core/TYPE)
          term (core/alg-term compose [f g] hom-type)
          retagged (scope/retag term {tag1 tag2})]
      (is (= tag2 (-> retagged :head :tag)))
      (is (= tag2 (-> retagged :args first :tag)))
      (is (= tag2 (-> retagged :type :head :tag))))))

(deftest test-type-ctx-retag
  (testing "TypeCtx retag works"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          x (scope/ident tag1 0 'x)
          ob (scope/ident tag1 1 'Ob)
          ob-type (core/alg-type ob [] core/TYPE)
          ctx (core/type-ctx [x] [ob-type])
          retagged (scope/retag ctx {tag1 tag2})]
      (is (= tag2 (-> retagged :idents first :tag)))
      (is (= tag2 (-> retagged :types first :head :tag))))))

(deftest test-gat-rename
  (testing "GAT rename only affects its own scope"
    (let [tag (scope/scope-tag)
          ob (scope/ident tag 0 'Ob)
          ob-type (core/alg-type ob [] core/TYPE)
          tic (core/type-in-ctx (core/type-ctx) ob-type)
          gat (-> (core/empty-gat 'TestTheory)
                  (assoc :tag tag)
                  (core/add-type-constructor tic))
          renamed (scope/rename gat tag {'Ob 'Object})]
      (is (= 'Object (-> renamed
                         :type-constructors
                         first
                         :type
                         :head
                         :name))))))

;;; ============================================================================
;;; Integration Test: Building a Simple Category Theory
;;; ============================================================================

(deftest test-build-simple-category
  (testing "Can build a simple category theory"
    (let [;; Create theory with fresh scope
          tag (scope/scope-tag)
          gat (-> (core/empty-gat 'Category)
                  (assoc :tag tag))

          ;; Add Ob : TYPE
          ob-ident (scope/ident tag 0 'Ob)
          ob-type (core/alg-type ob-ident [] core/TYPE)
          ob-tic (core/type-in-ctx (core/type-ctx) ob-type)
          gat (core/add-type-constructor gat ob-tic)

          ;; Add Hom : (Ob, Ob) → TYPE
          hom-ident (scope/ident tag 1 'Hom)
          dom-ident (scope/ident tag 2 'dom)
          cod-ident (scope/ident tag 3 'cod)
          hom-ctx (-> (core/type-ctx)
                      (core/add-binding dom-ident ob-type)
                      (core/add-binding cod-ident ob-type))
          hom-type (core/alg-type hom-ident [dom-ident cod-ident] core/TYPE)
          hom-tic (core/type-in-ctx hom-ctx hom-type)
          gat (core/add-type-constructor gat hom-tic)

          ;; Add id : (a : Ob) → Hom(a, a)
          id-ident (scope/ident tag 4 'id)
          a-ident (scope/ident tag 5 'a)
          id-ctx (core/add-binding (core/type-ctx) a-ident ob-type)
          id-ret-type (core/alg-type hom-ident [a-ident a-ident] core/TYPE)
          id-term (core/alg-term id-ident [a-ident] id-ret-type)
          id-tic (core/term-in-ctx id-ctx id-term)
          gat (core/add-term-constructor gat id-tic)]

      ;; Verify the theory
      (is (= 'Category (:name gat)))
      (is (= 2 (count (:type-constructors gat))))
      (is (= 1 (count (:term-constructors gat))))

      ;; Verify we can find components
      (is (some? (core/get-type-constructor gat 'Ob)))
      (is (some? (core/get-type-constructor gat 'Hom)))
      (is (some? (core/get-term-constructor gat 'id)))

      ;; Verify structure of Hom
      (let [hom-found (core/get-type-constructor gat 'Hom)]
        (is (= 2 (core/context-length (:ctx hom-found))))
        (is (= 2 (count (-> hom-found :type :args))))))))
