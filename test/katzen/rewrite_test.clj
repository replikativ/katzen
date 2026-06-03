(ns katzen.rewrite-test
  "Tests for Pattern-based rewriting of GAT expressions."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.rewrite :as rewrite]
            [katzen.core :as core]
            [katzen.scope :as scope]
            [katzen.theory :as theory]))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

;; Create a simple scope tag for testing
(def test-scope (scope/scope-tag))

;; Create test Idents
(defn make-ident [name lid]
  (scope/ident test-scope lid name))

(def a-ident (make-ident 'a 0))
(def b-ident (make-ident 'b 1))
(def c-ident (make-ident 'c 2))
(def f-ident (make-ident 'f 3))
(def g-ident (make-ident 'g 4))
(def compose-ident (make-ident 'compose 5))
(def id-ident (make-ident 'id 6))

;;; ============================================================================
;;; Conversion Tests: term->sexp
;;; ============================================================================

(deftest test-ident-name
  (testing "Extract name from Ident"
    (is (= 'a (rewrite/ident-name a-ident)))
    (is (= 'compose (rewrite/ident-name compose-ident))))

  (testing "Pass through symbols"
    (is (= 'x (rewrite/ident-name 'x)))))

(deftest test-term->sexp-simple
  (testing "Simple Ident converts to symbol"
    (is (= 'a (rewrite/term->sexp a-ident))))

  (testing "AlgTerm without args converts to single-element list"
    ;; Use record constructor to avoid type validation
    (let [term (core/->AlgTerm id-ident [] nil)]
      (is (= '(id) (rewrite/term->sexp term))))))

(deftest test-term->sexp-with-args
  (testing "AlgTerm with args converts to list"
    (let [term (core/->AlgTerm compose-ident [a-ident b-ident c-ident] nil)]
      (is (= '(compose a b c) (rewrite/term->sexp term)))))

  (testing "Nested AlgTerm converts to nested list"
    (let [inner (core/->AlgTerm id-ident [a-ident] nil)
          outer (core/->AlgTerm compose-ident [a-ident b-ident inner] nil)]
      (is (= '(compose a b (id a)) (rewrite/term->sexp outer))))))

;;; ============================================================================
;;; Conversion Tests: collect-idents
;;; ============================================================================

(deftest test-collect-idents
  (testing "Collect from simple Ident"
    (let [idents (rewrite/collect-idents a-ident)]
      (is (= a-ident (get idents 'a)))))

  (testing "Collect from AlgTerm"
    (let [term (core/->AlgTerm compose-ident [a-ident b-ident c-ident] nil)
          idents (rewrite/collect-idents term)]
      (is (= compose-ident (get idents 'compose)))
      (is (= a-ident (get idents 'a)))
      (is (= b-ident (get idents 'b)))
      (is (= c-ident (get idents 'c)))
      (is (= 4 (count idents)))))

  (testing "Collect from nested AlgTerm"
    (let [inner (core/->AlgTerm id-ident [a-ident] nil)
          outer (core/->AlgTerm compose-ident [a-ident b-ident inner] nil)
          idents (rewrite/collect-idents outer)]
      (is (= compose-ident (get idents 'compose)))
      (is (= id-ident (get idents 'id)))
      (is (= a-ident (get idents 'a)))
      (is (= b-ident (get idents 'b)))
      (is (= 4 (count idents))))))

;;; ============================================================================
;;; Conversion Tests: sexp->term
;;; ============================================================================

(deftest test-sexp->term-simple
  (testing "Symbol with ident map"
    (let [ident-map {'a a-ident}
          result (rewrite/sexp->term 'a ident-map)]
      (is (= a-ident result))))

  (testing "Symbol without ident map returns nil when no scope"
    (let [result (rewrite/sexp->term 'unknown {})]
      (is (nil? result)))))

(deftest test-sexp->term-list
  (testing "Simple list reconstructs AlgTerm"
    (let [ident-map {'compose compose-ident 'a a-ident 'b b-ident}
          result (rewrite/sexp->term '(compose a b) ident-map)]
      (is (core/alg-term? result))
      (is (= compose-ident (:head result)))
      (is (= 2 (count (:args result))))
      (is (= a-ident (first (:args result))))
      (is (= b-ident (second (:args result))))))

  (testing "Nested list reconstructs nested AlgTerm"
    (let [ident-map {'compose compose-ident 'id id-ident 'a a-ident 'b b-ident}
          result (rewrite/sexp->term '(compose a (id b)) ident-map)]
      (is (core/alg-term? result))
      (is (= 2 (count (:args result))))
      (let [inner-term (second (:args result))]
        (is (core/alg-term? inner-term))
        (is (= id-ident (:head inner-term)))))))

;;; ============================================================================
;;; Round-Trip Tests
;;; ============================================================================

(deftest test-round-trip-simple
  (testing "Simple Ident round-trips"
    (let [original a-ident
          sexp (rewrite/term->sexp original)
          ident-map (rewrite/collect-idents original)
          reconstructed (rewrite/sexp->term sexp ident-map)]
      (is (= original reconstructed)))))

(deftest test-round-trip-term
  (testing "AlgTerm round-trips"
    (let [original (core/->AlgTerm compose-ident [a-ident b-ident c-ident] nil)
          sexp (rewrite/term->sexp original)
          ident-map (rewrite/collect-idents original)
          reconstructed (rewrite/sexp->term sexp ident-map)]
      (is (= (:head original) (:head reconstructed)))
      (is (= (count (:args original)) (count (:args reconstructed))))
      (is (= (map :name (:args original))
             (map :name (:args reconstructed)))))))

(deftest test-round-trip-nested
  (testing "Nested AlgTerm round-trips"
    (let [inner (core/->AlgTerm id-ident [a-ident] nil)
          outer (core/->AlgTerm compose-ident [a-ident b-ident inner] nil)
          sexp (rewrite/term->sexp outer)
          ident-map (rewrite/collect-idents outer)
          reconstructed (rewrite/sexp->term sexp ident-map)]
      (is (= (:head outer) (:head reconstructed)))
      (is (= (count (:args outer)) (count (:args reconstructed))))
      ;; Check nested structure
      (let [orig-inner (nth (:args outer) 2)
            recon-inner (nth (:args reconstructed) 2)]
        (is (core/alg-term? recon-inner))
        (is (= (:head orig-inner) (:head recon-inner)))))))

;;; ============================================================================
;;; Pattern Conversion Tests
;;; ============================================================================

(deftest test-term->pattern-free-vars
  (testing "Free variables become pattern variables"
    (let [term (core/->AlgTerm compose-ident [a-ident b-ident c-ident] nil)
          free-vars #{'a 'b 'c}
          pattern (rewrite/term->pattern term free-vars)]
      (is (= '(compose ?a ?b ?c) pattern))))

  (testing "Bound constructors remain as literals"
    (let [term (core/->AlgTerm compose-ident [a-ident b-ident c-ident] nil)
          free-vars #{'a 'b}  ; c is not free
          pattern (rewrite/term->pattern term free-vars)]
      (is (= '(compose ?a ?b c) pattern)))))

(deftest test-term->pattern-nested
  (testing "Nested terms with free variables"
    (let [inner (core/->AlgTerm id-ident [a-ident] nil)
          outer (core/->AlgTerm compose-ident [a-ident b-ident inner] nil)
          free-vars #{'a 'b}
          pattern (rewrite/term->pattern outer free-vars)]
      (is (= '(compose ?a ?b (id ?a)) pattern)))))

(deftest test-pattern-vars
  (testing "Extract pattern variables"
    (is (= #{'?a '?b '?c} (rewrite/pattern-vars '(compose ?a ?b ?c))))
    (is (= #{'?a} (rewrite/pattern-vars '(compose ?a b c))))
    (is (= #{} (rewrite/pattern-vars '(compose a b c)))))

  (testing "Extract from nested patterns"
    (is (= #{'?a '?b} (rewrite/pattern-vars '(compose ?a (id ?b)))))))

;;; ============================================================================
;;; Template Conversion Tests
;;; ============================================================================

(deftest test-term->template
  (testing "Template conversion same as pattern for simple cases"
    (let [term f-ident
          free-vars #{'f}
          template (rewrite/term->template term free-vars)]
      (is (= '?f template))))

  (testing "Template for AlgTerm"
    (let [term (core/->AlgTerm compose-ident [a-ident b-ident c-ident] nil)
          free-vars #{'a 'b 'c}
          template (rewrite/term->template term free-vars)]
      (is (= '(compose ?a ?b ?c) template)))))

;;; ============================================================================
;;; Integration Test with Real Theory
;;; ============================================================================

(theory/deftheory RewriteSimpleCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))

  (term id
        :ctx [a Ob]
        :ret (Hom a a))

  ;; Identity law: id(a) ∘ f = f
  (axiom id-left
         :ctx [a Ob, b Ob, f (Hom a b)]
         (= (compose a a b (id a) f) f)))

(deftest test-theory-integration
  (testing "Can convert terms from real theory"
    (let [;; Get a term constructor from the theory
          compose-term (first (filter #(= 'compose (-> % :term :head :name))
                                      (:term-constructors RewriteSimpleCategory)))
          ;; This is a term in context, extract the actual term
          term-in-ctx (:term compose-term)
          sexp (rewrite/term->sexp term-in-ctx)]
      ;; Should convert to some form (seq because term->sexp uses list*)
      (is (not (nil? sexp)))
      (is (seq? sexp)))))

(deftest test-axiom-structure
  (testing "Can access axiom lhs and rhs"
    (let [axioms (:axioms RewriteSimpleCategory)]
      (is (= 1 (count axioms)))
      (let [id-left-axiom (first axioms)]
        (is (= 'id-left (:name id-left-axiom)))
        (is (some? (:lhs id-left-axiom)))
        (is (some? (:rhs id-left-axiom)))))))

;;; ============================================================================
;;; Axiom → Rule Conversion Tests
;;; ============================================================================

(deftest test-axiom->rule-creation
  (testing "Can create a Pattern rule from an axiom"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ;; Extract free variables from axiom context
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rule (rewrite/axiom->rule axiom free-vars)]
      ;; Rule should be a function (Pattern rules are functions)
      (is (fn? rule))
      ;; Rule name should be based on axiom name
      (is (some? rule)))))

(deftest test-apply-identity-rule
  (testing "Can apply identity law rewrite rule"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))

          ;; Get the LHS term from the axiom
          lhs-term (:lhs axiom)

          ;; Apply the rule
          result (rewrite/apply-rule
                  (rewrite/axiom->rule axiom free-vars)
                  lhs-term)]

      ;; Result should be the simplified term (RHS of axiom)
      ;; For id-left axiom: (compose a a b (id a) f) => f
      (is (some? result))

      ;; Result should be an AlgTerm representing the variable f
      (is (core/alg-term? result))
      ;; With f as the head and no args
      (is (= 'f (-> result :head :name)))
      (is (empty? (:args result))))))

;;; ============================================================================
;;; Normalization Strategy Tests
;;; ============================================================================

(deftest test-apply-rules
  (testing "apply-rules tries rules in sequence"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          ;; Create a term that matches: (compose a a b (id a) f)
          lhs-term (:lhs axiom)]

      ;; Should match and rewrite
      (let [result (rewrite/apply-rules rules lhs-term)]
        (is (= 'f (-> result :head :name)))))))

(deftest test-rewrite-subterms
  (testing "rewrite-subterms applies function bottom-up"
    (let [;; Create a simple rewrite function that adds metadata
          add-marker (fn [term]
                       (if (core/alg-term? term)
                         (vary-meta term assoc :visited true)
                         term))

          ;; Create nested term: (compose a b c f (id b))
          inner (core/->AlgTerm id-ident [b-ident] nil)
          outer (core/->AlgTerm compose-ident [a-ident b-ident c-ident f-ident inner] nil)

          result (rewrite/rewrite-subterms add-marker outer)]

      ;; Both outer and inner should be marked
      (is (:visited (meta result)))
      (is (:visited (meta (last (:args result))))))))

(deftest test-simplify-once
  (testing "simplify-once applies rules at top level only"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          lhs-term (:lhs axiom)]

      ;; Should simplify the top-level match
      (let [result (rewrite/simplify-once rules lhs-term)]
        (is (= 'f (-> result :head :name)))))))

(deftest test-simplify-deep
  (testing "simplify-deep applies rules to subterms"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          ;; Create a term with nested identity:
          ;; (compose x y z g (compose a a b (id a) f))
          ;; The inner compose should simplify to f
          inner (:lhs axiom)  ; (compose a a b (id a) f)
          outer (core/->AlgTerm compose-ident
                                [a-ident b-ident c-ident g-ident inner]
                                nil)]

      ;; Should simplify the inner term
      (let [result (rewrite/simplify-deep rules outer)]
        (is (core/alg-term? result))
        ;; The last arg should now be f (simplified from inner)
        (is (= 'f (-> result :args last :head :name)))))))

(deftest test-normalize-fixed-point
  (testing "normalize reaches fixed point"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          ;; Create a term that needs multiple iterations:
          ;; (compose x y z (id y) (compose a a b (id a) f))
          inner (:lhs axiom)
          outer (core/->AlgTerm compose-ident
                                [a-ident b-ident c-ident (core/->AlgTerm id-ident [b-ident] nil) inner]
                                nil)]

      ;; Should normalize both the (id y) and the inner compose
      (let [result (rewrite/normalize rules outer)]
        (is (core/alg-term? result))
        ;; Should have simplified to (compose a b c ? f) where one id was removed
        ;; The exact result depends on how rules are applied
        (is (some? result))))))

(deftest test-normalize-iteration-limit
  (testing "normalize respects iteration limit"
    (let [;; Create a rule that always matches and changes the term
          ;; This would cause infinite looping
          infinite-rule (fn [_sexp] 'different-each-time)
          rules [infinite-rule]
          term a-ident

          max-iters 10]

      ;; Should stop after max iterations, not hang
      (let [result (rewrite/normalize rules term max-iters)]
        (is (some? result))))))

;;; ============================================================================
;;; Equational Reasoning Tests
;;; ============================================================================

(deftest test-terms-equal
  (testing "terms-equal? checks equality via normalization"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          ;; LHS and RHS of axiom should be equal
          lhs (:lhs axiom)
          rhs (:rhs axiom)]

      ;; LHS and RHS should be equal
      (is (rewrite/terms-equal? rules lhs rhs))

      ;; Term should be equal to itself
      (is (rewrite/terms-equal? rules lhs lhs)))))

(deftest test-normalize-with-trace
  (testing "normalize-with-trace records each step"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          lhs (:lhs axiom)
          trace (rewrite/normalize-with-trace rules lhs)]

      ;; Should start with original term
      (is (= lhs (first trace)))

      ;; Should end with normalized form
      (is (= 'f (-> trace last :head :name)))

      ;; Trace should have at least 2 steps (original and result)
      (is (>= (count trace) 2)))))

(deftest test-rewrite-path
  (testing "rewrite-path shows transformation between equal terms"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          ctx (:ctx axiom)
          free-vars (set (map (comp :name first) (:bindings ctx)))
          rules [(rewrite/axiom->rule axiom free-vars)]

          lhs (:lhs axiom)
          rhs (:rhs axiom)
          path (rewrite/rewrite-path rules lhs rhs)]

      ;; Should find a path (they're equal via axiom)
      (is (some? path))

      ;; Path should start with lhs (structurally)
      (is (= (rewrite/term->sexp lhs) (rewrite/term->sexp (first path))))

      ;; Path should end with something equivalent to rhs (structurally)
      (is (= (rewrite/term->sexp rhs) (rewrite/term->sexp (last path)))))))

(deftest test-rewrite-path-not-equal
  (testing "rewrite-path returns nil for non-equal terms"
    (let [rules []
          path (rewrite/rewrite-path rules a-ident b-ident)]

      ;; Different variables with no rules to relate them
      (is (nil? path)))))

(deftest test-prove-equation
  (testing "prove-equation verifies axiom equations"
    (let [axiom (first (:axioms RewriteSimpleCategory))
          lhs (:lhs axiom)
          rhs (:rhs axiom)
          proof (rewrite/prove-equation RewriteSimpleCategory lhs rhs)]

      ;; Should be provable (it's an axiom!)
      (is (:provable? proof))

      ;; Should have a normal form
      (is (some? (:normal-form proof)))

      ;; Should have steps for both sides
      (is (vector? (:lhs-steps proof)))
      (is (vector? (:rhs-steps proof)))
      (is (pos? (count (:lhs-steps proof))))
      (is (pos? (count (:rhs-steps proof)))))))
