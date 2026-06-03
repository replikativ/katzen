(ns katzen.pretty-test
  "Tests for pretty printing GATs with unicode support."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.theory :as theory]
            [katzen.pretty :as pretty]
            [clojure.string :as str]))

;;; ============================================================================
;;; Test Theories
;;; ============================================================================

(theory/deftheory PrettySimpleCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))

  (term id
        :ctx [a Ob]
        :ret (Hom a a)))

(theory/deftheory UnicodeSMC
  (type Ob)
  (type → [dom Ob, codom Ob])

  (term ⋅
        :ctx [a Ob, b Ob, c Ob]
        :args [f (→ a b), g (→ b c)]
        :ret (→ a c))

  (term id
        :ctx [a Ob]
        :ret (→ a a))

  (term ⊗
        :ctx [a Ob, b Ob]
        :ret Ob)

  (term I
        :ret Ob))

;;; ============================================================================
;;; Type Expression Rendering Tests
;;; ============================================================================

(deftest test-render-type-expr-simple
  (testing "Simple type variables render correctly"
    (let [ob-type (first (filter #(= 'Ob (-> % :type :head :name))
                                 (:type-constructors PrettySimpleCategory)))
          ob-expr (:type ob-type)]
      ;; ASCII rendering
      (is (= "Ob" (pretty/render-type-expr ob-expr :unicode? false)))
      ;; Unicode rendering (Ob has no unicode form, stays as is)
      (is (= "Ob" (pretty/render-type-expr ob-expr :unicode? true))))))

(deftest test-render-type-expr-application
  (testing "Type applications render correctly"
    (let [hom-type (first (filter #(= 'Hom (-> % :type :head :name))
                                  (:type-constructors PrettySimpleCategory)))
          ;; Get the type of the compose term to test rendering
          compose-term (first (filter #(= 'compose (-> % :term :head :name))
                                      (:term-constructors PrettySimpleCategory)))
          hom-expr (-> compose-term :term :type)]
      ;; ASCII rendering
      (is (str/includes? (pretty/render-type-expr hom-expr :unicode? false) "Hom"))
      ;; Unicode rendering (Hom → →)
      (is (str/includes? (pretty/render-type-expr hom-expr :unicode? true) "→")))))

;;; ============================================================================
;;; Term Rendering Tests
;;; ============================================================================

(deftest test-render-term-ascii
  (testing "Terms render with ASCII names"
    (let [compose-term (first (filter #(= 'compose (-> % :term :head :name))
                                      (:term-constructors PrettySimpleCategory)))]
      (let [rendered (pretty/render-term compose-term :unicode? false :show-type? false)]
        (is (= "compose" rendered))))))

(deftest test-render-term-unicode
  (testing "Terms render with unicode names"
    (let [compose-term (first (filter #(= 'compose (-> % :term :head :name))
                                      (:term-constructors UnicodeSMC)))]
      (let [rendered (pretty/render-term compose-term :unicode? true :show-type? false)]
        (is (= "⋅" rendered))))))

;;; ============================================================================
;;; Theory Rendering Tests
;;; ============================================================================

(deftest test-render-theory-ascii
  (testing "Theory renders with ASCII syntax"
    (let [rendered (pretty/render-theory PrettySimpleCategory :unicode? false)]
      (is (str/includes? rendered "deftheory PrettySimpleCategory"))
      (is (str/includes? rendered "(type Ob)"))
      (is (str/includes? rendered "(type Hom"))
      (is (str/includes? rendered "(term compose"))
      (is (str/includes? rendered "(term id"))
      ;; Should use ASCII Hom, not →
      (is (str/includes? rendered "Hom"))
      (is (not (str/includes? rendered "→"))))))

(deftest test-render-theory-unicode
  (testing "Theory renders with unicode syntax"
    (let [rendered (pretty/render-theory UnicodeSMC :unicode? true)]
      (is (str/includes? rendered "deftheory UnicodeSMC"))
      (is (str/includes? rendered "(type Ob)"))
      ;; Should use → instead of Hom
      (is (str/includes? rendered "→"))
      ;; Should use ⋅ instead of compose
      (is (str/includes? rendered "⋅"))
      ;; Should use ⊗ instead of otimes
      (is (str/includes? rendered "⊗"))
      ;; Should use I instead of munit
      (is (str/includes? rendered "I")))))

(deftest test-round-trip
  (testing "ASCII theory can be rendered and would parse the same"
    (let [rendered (pretty/render-theory PrettySimpleCategory :unicode? false)]
      ;; Check that the rendered form contains the same structure
      (is (str/includes? rendered "SimpleCategory"))
      (is (str/includes? rendered "Hom"))
      (is (str/includes? rendered "compose")))))

;;; ============================================================================
;;; Theory Summary Tests
;;; ============================================================================

(deftest test-theory-summary-basic
  (testing "Theory summary shows basic info"
    (let [summary (pretty/theory-summary PrettySimpleCategory)]
      (is (str/includes? summary "Theory: PrettySimpleCategory"))
      (is (str/includes? summary "Types: 2"))
      (is (str/includes? summary "Terms: 2")))))

(deftest test-theory-summary-unicode
  (testing "Theory summary can use unicode"
    (let [summary (pretty/theory-summary UnicodeSMC :unicode? true)]
      (is (str/includes? summary "Theory: UnicodeSMC"))
      ;; Should show unicode operators in term names
      (is (str/includes? summary "⋅"))
      (is (str/includes? summary "⊗")))))

(deftest test-theory-summary-ascii
  (testing "Theory summary can use ASCII"
    (let [summary (pretty/theory-summary UnicodeSMC :unicode? false)]
      (is (str/includes? summary "Theory: UnicodeSMC"))
      ;; Should show ASCII names even though theory uses unicode
      (is (str/includes? summary "compose"))
      (is (str/includes? summary "otimes")))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(deftest test-empty-context
  (testing "Terms with no context render correctly"
    (let [i-term (first (filter #(= 'munit (-> % :term :head :name))
                                (:term-constructors UnicodeSMC)))
          rendered (pretty/render-term-constructor i-term :unicode? true)]
      (is (str/includes? rendered "I"))
      (is (str/includes? rendered ":ret Ob")))))

(deftest test-mixed-unicode-ascii
  (testing "Can render same theory with both unicode and ASCII"
    (let [unicode-render (pretty/render-theory UnicodeSMC :unicode? true)
          ascii-render (pretty/render-theory UnicodeSMC :unicode? false)]
      ;; Unicode version uses symbols
      (is (str/includes? unicode-render "⋅"))
      (is (str/includes? unicode-render "→"))
      ;; ASCII version uses names
      (is (str/includes? ascii-render "compose"))
      (is (str/includes? ascii-render "Hom")))))
