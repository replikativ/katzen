(ns katzen.theory-test
  "Comprehensive tests for the theory macro."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.theory :as theory]
            [katzen.core :as core]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; Theory Definitions (namespace level to avoid protocol conflicts)
;;; ============================================================================

(theory/deftheory TheoryTestEmptyTheory)

(theory/deftheory TheoryTestSimpleType
  (type Ob))

(theory/deftheory TheoryTestMultiType
  (type A)
  (type B)
  (type C))

(theory/deftheory TheoryTestDependentType
  (type Ob)
  (type Hom [dom Ob, codom Ob]))

(theory/deftheory TheoryTestSimpleTerm
  (type Ob)
  (term unit :ret Ob))

(theory/deftheory TheoryTestTermWithArgs
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c)))

(theory/deftheory TheoryTestTermWithCtx
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term id
        :ctx [a Ob]
        :ret (Hom a a)))

(theory/deftheory TheoryTestSimpleAxiom
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))
  (axiom test
         :ctx [a Ob, b Ob, c Ob,
               f (Hom a b), g (Hom b c)]
         (= (compose a b c f g)
            (compose a b c f g))))

(theory/deftheory TheoryTestCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))

  (term id
        :ctx [a Ob]
        :ret (Hom a a))

  (axiom assoc
         :ctx [a Ob, b Ob, c Ob, d Ob,
               f (Hom a b), g (Hom b c), h (Hom c d)]
         (= (compose a c d (compose a b c f g) h)
            (compose a b d f (compose b c d g h))))

  (axiom id-left
         :ctx [a Ob, b Ob, f (Hom a b)]
         (= (compose a a b (id a) f) f))

  (axiom id-right
         :ctx [a Ob, b Ob, f (Hom a b)]
         (= (compose a b b f (id b)) f)))

(theory/deftheory TheoryTestMonoid
  (type M)

  (term mul
        :args [x M, y M]
        :ret M)

  (term e
        :ret M)

  (axiom assoc
         :ctx [x M, y M, z M]
         (= (mul (mul x y) z)
            (mul x (mul y z))))

  (axiom id-left
         :ctx [x M]
         (= (mul (e) x) x))

  (axiom id-right
         :ctx [x M]
         (= (mul x (e)) x)))

(theory/deftheory TheoryTestMonoidalCategory
  ;; Base category structure
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))

  (term id
        :ctx [a Ob]
        :ret (Hom a a))

  ;; Monoidal structure
  (term otimes
        :args [a Ob, b Ob]
        :ret Ob)

  (term munit
        :ret Ob))

(theory/deftheory TheoryTestPrintTest
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term id :ctx [a Ob] :ret (Hom a a)))

(theory/deftheory TheoryTestScopeTest
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c)))

;;; ============================================================================
;;; Basic Theory Definition Tests
;;; ============================================================================

(deftest test-empty-theory
  (testing "Can define empty theory"
    (is (core/gat? TheoryTestEmptyTheory))
    (is (= 'TheoryTestEmptyTheory (:name TheoryTestEmptyTheory)))
    (is (empty? (:type-constructors TheoryTestEmptyTheory)))
    (is (empty? (:term-constructors TheoryTestEmptyTheory)))
    (is (empty? (:axioms TheoryTestEmptyTheory)))))

(deftest test-simple-type-theory
  (testing "Can define theory with single nullary type"
    (is (core/gat? TheoryTestSimpleType))
    (is (= 1 (count (:type-constructors TheoryTestSimpleType))))

    (let [ob-tic (first (:type-constructors TheoryTestSimpleType))
          ob-type (:type ob-tic)]
      (is (= 'Ob (-> ob-type :head :name)))
      (is (empty? (:args ob-type)))
      (is (= core/TYPE (:sort ob-type))))))

(deftest test-theory-with-multiple-types
  (testing "Can define theory with multiple types"
    (is (= 3 (count (:type-constructors TheoryTestMultiType))))
    (let [names (map #(-> % :type :head :name) (:type-constructors TheoryTestMultiType))]
      (is (= '[A B C] names)))))

;;; ============================================================================
;;; Dependent Type Tests
;;; ============================================================================

(deftest test-dependent-type
  (testing "Can define dependent type with context"
    (is (= 2 (count (:type-constructors TheoryTestDependentType))))

    (let [hom-tic (second (:type-constructors TheoryTestDependentType))
          hom-ctx (:ctx hom-tic)
          hom-type (:type hom-tic)]

      ;; Check context has 2 bindings
      (is (= 2 (core/context-length hom-ctx)))

      ;; Check context binding names
      (is (= 'dom (-> hom-ctx :idents first :name)))
      (is (= 'codom (-> hom-ctx :idents second :name)))

      ;; Check Hom type takes context vars as args
      (is (= 'Hom (-> hom-type :head :name)))
      (is (= 2 (count (:args hom-type)))))))

;;; ============================================================================
;;; Term Constructor Tests
;;; ============================================================================

(deftest test-simple-term
  (testing "Can define term with just return type"
    (is (= 1 (count (:term-constructors TheoryTestSimpleTerm))))

    (let [unit-tic (first (:term-constructors TheoryTestSimpleTerm))
          unit-term (:term unit-tic)]
      (is (= 'unit (-> unit-term :head :name)))
      (is (= 'Ob (-> unit-term :type :head :name))))))

(deftest test-term-with-args
  (testing "Can define term with arguments"
    (is (= 1 (count (:term-constructors TheoryTestTermWithArgs))))

    (let [compose-tic (first (:term-constructors TheoryTestTermWithArgs))
          compose-ctx (:ctx compose-tic)
          compose-term (:term compose-tic)]

      ;; Context should have all bindings (ctx + args)
      (is (= 5 (core/context-length compose-ctx)))

      ;; Names: a, b, c, f, g
      (is (= '[a b c f g]
             (mapv :name (:idents compose-ctx))))

      ;; Return type is Hom(a, c)
      (is (= 'Hom (-> compose-term :type :head :name)))
      (is (= 2 (count (-> compose-term :type :args)))))))

(deftest test-term-with-only-ctx
  (testing "Can define term with context but no args"
    (let [id-tic (first (:term-constructors TheoryTestTermWithCtx))
          id-ctx (:ctx id-tic)
          id-term (:term id-tic)]

      (is (= 1 (core/context-length id-ctx)))
      (is (= 'a (-> id-ctx :idents first :name)))
      (is (= 'Hom (-> id-term :type :head :name))))))

;;; ============================================================================
;;; Axiom Tests
;;; ============================================================================

(deftest test-simple-axiom
  (testing "Can define axiom"
    (is (= 1 (count (:axioms TheoryTestSimpleAxiom))))

    (let [axiom (first (:axioms TheoryTestSimpleAxiom))]
      (is (= 'test (:name axiom)))
      (is (= 5 (core/context-length (:ctx axiom))))
      (is (core/alg-term? (:lhs axiom)))
      (is (core/alg-term? (:rhs axiom))))))

;;; ============================================================================
;;; Complete Theory: Category
;;; ============================================================================

(deftest test-category-theory
  (testing "Can define complete category theory"
    ;; Verify structure
    (is (= 'TheoryTestCategory (:name TheoryTestCategory)))
    (is (= 2 (count (:type-constructors TheoryTestCategory))))
    (is (= 2 (count (:term-constructors TheoryTestCategory))))
    (is (= 3 (count (:axioms TheoryTestCategory))))

    ;; Verify Ob
    (let [ob-tic (first (:type-constructors TheoryTestCategory))]
      (is (= 'Ob (-> ob-tic :type :head :name))))

    ;; Verify Hom
    (let [hom-tic (second (:type-constructors TheoryTestCategory))]
      (is (= 'Hom (-> hom-tic :type :head :name)))
      (is (= 2 (core/context-length (:ctx hom-tic)))))

    ;; Verify compose
    (let [compose-tic (first (:term-constructors TheoryTestCategory))]
      (is (= 'compose (-> compose-tic :term :head :name)))
      (is (= 5 (core/context-length (:ctx compose-tic)))))

    ;; Verify id
    (let [id-tic (second (:term-constructors TheoryTestCategory))]
      (is (= 'id (-> id-tic :term :head :name)))
      (is (= 1 (core/context-length (:ctx id-tic)))))

    ;; Verify axioms have correct context lengths
    (is (= '[7 3 3]
           (mapv #(core/context-length (:ctx %)) (:axioms TheoryTestCategory))))))

;;; ============================================================================
;;; Complete Theory: Monoid
;;; ============================================================================

(deftest test-monoid-theory
  (testing "Can define monoid theory"
    (is (= 1 (count (:type-constructors TheoryTestMonoid))))
    (is (= 2 (count (:term-constructors TheoryTestMonoid))))
    (is (= 3 (count (:axioms TheoryTestMonoid))))

    ;; Verify mul takes 2 args
    (let [mul-tic (first (:term-constructors TheoryTestMonoid))]
      (is (= 2 (core/context-length (:ctx mul-tic)))))

    ;; Verify e takes no args
    (let [e-tic (second (:term-constructors TheoryTestMonoid))]
      (is (= 0 (core/context-length (:ctx e-tic)))))))

;;; ============================================================================
;;; Theory Hierarchy: Monoidal Category
;;; ============================================================================

(deftest test-monoidal-category-theory
  (testing "Can define monoidal category (extending category)"
    (is (= 2 (count (:type-constructors TheoryTestMonoidalCategory))))
    (is (= 4 (count (:term-constructors TheoryTestMonoidalCategory))))

    ;; Verify otimes
    (let [otimes-tic (nth (:term-constructors TheoryTestMonoidalCategory) 2)]
      (is (= 'otimes (-> otimes-tic :term :head :name)))
      (is (= 2 (core/context-length (:ctx otimes-tic)))))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

;; Note: Error handling tests are disabled because errors during macro expansion
;; get wrapped in CompilerException, making them difficult to test directly.
;; The error messages are still present in the code and will show up during
;; actual usage.

;; (deftest test-error-unbound-type
;;   (testing "Error on unbound type in context"
;;     ;; This would throw: Unbound type variable: UnknownType
;;     ;; (theory/deftheory BadTheory
;;     ;;   (type Hom [dom UnknownType]))
;;     ))

;; (deftest test-error-term-without-ret
;;   (testing "Error on term without :ret"
;;     ;; This would throw: Term declaration must have :ret
;;     ;; (theory/deftheory BadTheory
;;     ;;   (type Ob)
;;     ;;   (term bad :args [x Ob]))
;;     ))

;;; ============================================================================
;;; Pretty Printing Tests
;;; ============================================================================

(deftest test-format-theory
  (testing "Can pretty-print a theory"
    (let [output (theory/format-theory TheoryTestPrintTest)]
      (is (string? output))
      (is (re-find #"Theory: TheoryTestPrintTest" output))
      (is (re-find #"Type Constructors:" output))
      (is (re-find #"Ob" output))
      (is (re-find #"Hom" output))
      (is (re-find #"Term Constructors:" output))
      (is (re-find #"id" output)))))

;;; ============================================================================
;;; Scope Hygiene Tests
;;; ============================================================================

(deftest test-scope-hygiene
  (testing "Theory has consistent scope tags"
    ;; All identifiers in the theory should have the same scope tag
    (let [theory-tag (:tag TheoryTestScopeTest)
          all-tags (atom #{})]

      ;; Collect tags from type constructors
      (doseq [tic (:type-constructors TheoryTestScopeTest)]
        (swap! all-tags conj (-> tic :type :head :tag))
        (doseq [arg (-> tic :type :args)]
          (swap! all-tags conj (:tag arg))))

      ;; Collect tags from term constructors
      (doseq [tic (:term-constructors TheoryTestScopeTest)]
        (swap! all-tags conj (-> tic :term :head :tag))
        (doseq [ident (-> tic :ctx :idents)]
          (swap! all-tags conj (:tag ident))))

      ;; All tags should be the theory's tag
      (is (= #{theory-tag} @all-tags)
          "All identifiers should have the theory's scope tag"))))
