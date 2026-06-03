(ns katzen.rewrite
  "Pattern-based rewriting for GAT expressions.

  This namespace integrates the Pattern library to enable:
  - Converting AlgTerm expressions to/from S-expressions
  - Creating rewrite rules from axioms
  - Normalizing terms using category theory laws
  - Equational reasoning and proof generation"
  (:require [katzen.core :as core]
            [katzen.scope :as scope]
            [clojure.set :as set]))

;;; ============================================================================
;;; AlgTerm ↔ S-Expression Conversion
;;; ============================================================================

(defn ident-name
  "Extract symbol name from an Ident, or return symbol as-is."
  [x]
  (if (scope/gat-ident? x)
    (:name x)
    x))

(defn term->sexp
  "Convert AlgTerm to S-expression for pattern matching.

  Recursively converts AlgTerm structures to lists with symbol names.
  Preserves the structure but loses scope information (tags).

  Example:
    AlgTerm{:head compose :args [a b c f g]}
    => '(compose a b c f g)"
  [term]
  (cond
    ;; AlgTerm with args - convert to list
    (core/alg-term? term)
    (list* (ident-name (:head term))
           (map term->sexp (:args term)))

    ;; Ident - extract name
    (scope/gat-ident? term)
    (ident-name term)

    ;; Already a symbol or other value
    :else
    term))

(defn collect-idents
  "Collect all Idents from a term into a name->ident map.

  This preserves scope information needed for reconstruction."
  [term]
  (cond
    (core/alg-term? term)
    (merge {(ident-name (:head term)) (:head term)}
           (apply merge (map collect-idents (:args term))))

    (scope/gat-ident? term)
    {(ident-name term) term}

    :else
    {}))

(defn sexp->term
  "Reconstruct AlgTerm from S-expression using ident map.

  The ident-map provides the original Idents with scope information.
  For symbols not in the map, creates new Idents with the current scope.

  Args:
    sexp - S-expression (list or symbol)
    ident-map - Map from symbol name to Ident
    scope-ctx - Scope context for creating new Idents (optional)

  Returns:
    AlgTerm or Ident"
  ([sexp ident-map]
   (sexp->term sexp ident-map nil))
  ([sexp ident-map scope-ctx]
   (cond
     ;; List/Seq - reconstruct AlgTerm
     (seq? sexp)
     (let [head-name (first sexp)
           head-ident (or (get ident-map head-name)
                          (when scope-ctx
                            ;; Create new ident if not found and scope provided
                            (scope/bind scope-ctx head-name)))
           args (map #(sexp->term % ident-map scope-ctx) (rest sexp))]
       (when head-ident
         ;; Note: Type needs to be inferred or provided separately
         ;; Use record constructor to avoid type validation for now
         (core/->AlgTerm head-ident (vec args) nil)))

     ;; Symbol - lookup or create Ident
     (symbol? sexp)
     (or (get ident-map sexp)
         (when scope-ctx
           (scope/bind scope-ctx sexp)))

     ;; Other value
     :else
     sexp)))

;;; ============================================================================
;;; Pattern Conversion
;;; ============================================================================

(defn term->pattern
  "Convert AlgTerm to Pattern pattern with match variables.

  Free variables (from context) become pattern variables (?var).
  Bound term constructors remain as literals.

  Example:
    Term: (compose a b c f g)
    Where: compose is a term constructor, a b c f g are free
    => '(compose ?a ?b ?c ?f ?g)"
  [term free-vars]
  (cond
    (core/alg-term? term)
    (let [head-name (ident-name (:head term))
          arg-patterns (map #(term->pattern % free-vars) (:args term))]
      (list* head-name arg-patterns))

    (scope/gat-ident? term)
    (let [name (ident-name term)]
      (if (contains? free-vars name)
        (symbol (str "?" name))  ; Free variable -> ?var
        name))                    ; Bound constructor

    :else
    term))

(defn pattern-vars
  "Extract all pattern variables (?var) from a pattern."
  [pattern]
  (cond
    (and (symbol? pattern)
         (.startsWith (name pattern) "?"))
    #{pattern}

    (list? pattern)
    (apply set/union (map pattern-vars pattern))

    :else
    #{}))

;;; ============================================================================
;;; Substitution Template Conversion
;;; ============================================================================

(defn term->template
  "Convert AlgTerm to Pattern substitution template.

  Similar to term->pattern, but used for the RHS of rewrite rules.

  Example:
    Term: f
    Free vars: {f}
    => '?f"
  [term free-vars]
  (cond
    (core/alg-term? term)
    (let [head-name (ident-name (:head term))
          arg-templates (map #(term->template % free-vars) (:args term))]
      (list* head-name arg-templates))

    (scope/gat-ident? term)
    (let [name (ident-name term)]
      (if (contains? free-vars name)
        (symbol (str "?" name))
        name))

    :else
    term))

;;; ============================================================================
;;; Pattern Matching and Substitution
;;; ============================================================================

(defn match-pattern
  "Manually match a pattern against an S-expression.

  Returns a map of bindings from pattern variables to values, or nil if no match.

  Example:
    (match-pattern '(compose ?a ?b ?c) '(compose x y z))
    => {'?a 'x, '?b 'y, '?c 'z}"
  [pattern sexp]
  (cond
    ;; Pattern variable - bind it
    (and (symbol? pattern)
         (.startsWith (name pattern) "?"))
    {pattern sexp}

    ;; Both lists/seqs - match recursively
    (and (seq? pattern) (seq? sexp))
    (when (= (count pattern) (count sexp))
      (let [bindings (map match-pattern pattern sexp)]
        (when (every? some? bindings)
          (apply merge bindings))))

    ;; Literals - must be equal
    (= pattern sexp)
    {}

    ;; No match
    :else
    nil))

(defn substitute-template
  "Substitute bindings into a template.

  Example:
    (substitute-template '?a {'?a 'x})
    => 'x

    (substitute-template '(compose ?a ?b) {'?a 'x, '?b 'y})
    => '(compose x y)"
  [template bindings]
  (cond
    ;; Pattern variable - lookup in bindings
    (and (symbol? template)
         (.startsWith (name template) "?"))
    (get bindings template template)

    ;; List - recursively substitute
    (seq? template)
    (map #(substitute-template % bindings) template)

    ;; Literal - return as-is
    :else
    template))

;;; ============================================================================
;;; Axiom → Rewrite Rule Conversion
;;; ============================================================================

(defn axiom-terms
  "Extract LHS and RHS from an axiom.

  Axioms have :lhs and :rhs fields directly."
  [axiom]
  [(:lhs axiom) (:rhs axiom)])

(defn axiom->rule
  "Convert an axiom to a rewrite rule function.

  An axiom is an equation between two terms.
  The rewrite rule will match the LHS pattern and substitute the RHS.

  Args:
    axiom - Axiom with :lhs and :rhs fields
    free-vars - Set of free variable names from axiom context

  Returns:
    Function that takes an S-expression and returns rewritten S-expression or nil

  Example:
    Axiom: (= (compose a a b (id a) f) f)
    Free vars: #{a b f}
    =>
    Rule function that matches '(compose a a b (id a) f) => 'f"
  [axiom free-vars]
  (let [[lhs rhs] (axiom-terms axiom)
        lhs-pattern (term->pattern lhs free-vars)
        rhs-template (term->template rhs free-vars)]
    (fn [sexp]
      (when-let [bindings (match-pattern lhs-pattern sexp)]
        (substitute-template rhs-template bindings)))))

(defn theory->rules
  "Extract all axiom rewrite rules from a theory.

  Returns a vector of rewrite rules, one per axiom."
  [theory]
  (vec
   (for [axiom (:axioms theory)]
     (let [;; Extract free variables from axiom context
           ctx (:ctx axiom)
           free-vars (set (map (comp :name first) (:bindings ctx)))]
       (axiom->rule axiom free-vars)))))

;;; ============================================================================
;;; Rule Application
;;; ============================================================================

(defn apply-rule
  "Apply a rewrite rule to a term at the top level.

  Returns the rewritten term if the rule matches, otherwise nil.

  Args:
    rule - Rewrite rule function (from axiom->rule)
    term - AlgTerm to rewrite

  Returns:
    Rewritten AlgTerm or nil if rule doesn't match"
  [rule term]
  (let [sexp (term->sexp term)
        ident-map (collect-idents term)
        result-sexp (rule sexp)]
    (when result-sexp
      (sexp->term result-sexp ident-map))))

(defn apply-rules
  "Try applying each rule in sequence until one matches.

  Returns the rewritten term from the first matching rule, or the original
  term if no rules match.

  Args:
    rules - Sequence of rewrite rule functions
    term - AlgTerm to rewrite

  Returns:
    Rewritten AlgTerm or original term if no match"
  [rules term]
  (or (some #(apply-rule % term) rules)
      term))

(defn rewrite-subterms
  "Recursively apply a rewrite function to all subterms.

  Applies the rewrite function bottom-up (children before parent).

  Args:
    rewrite-fn - Function that takes a term and returns a rewritten term
    term - AlgTerm to rewrite

  Returns:
    AlgTerm with all subterms rewritten"
  [rewrite-fn term]
  (if (core/alg-term? term)
    (let [;; First rewrite all arguments (bottom-up)
          rewritten-args (mapv #(rewrite-subterms rewrite-fn %) (:args term))
          ;; Reconstruct term with rewritten args
          term-with-new-args (core/->AlgTerm (:head term) rewritten-args (:type term))]
      ;; Then apply rewrite at this level
      (rewrite-fn term-with-new-args))
    ;; Not an AlgTerm, return as-is
    term))

;;; ============================================================================
;;; Normalization Strategies
;;; ============================================================================

(defn simplify-once
  "Apply rules once at the top level only.

  Args:
    rules - Sequence of rewrite rules
    term - AlgTerm to simplify

  Returns:
    Term after one application of rules (may be unchanged)"
  [rules term]
  (apply-rules rules term))

(defn simplify-deep
  "Apply rules to all subterms, bottom-up.

  Each subterm is simplified once, starting from the leaves.

  Args:
    rules - Sequence of rewrite rules
    term - AlgTerm to simplify

  Returns:
    Term with all subterms simplified once"
  [rules term]
  (rewrite-subterms #(apply-rules rules %) term))

(defn normalize
  "Apply rules repeatedly until reaching a fixed point.

  This repeatedly applies simplify-deep until the term stops changing.
  Includes a maximum iteration limit to prevent infinite loops.

  Args:
    rules - Sequence of rewrite rules
    term - AlgTerm to normalize
    max-iterations - Maximum number of iterations (default: 100)

  Returns:
    Normalized term (fixed point)"
  ([rules term]
   (normalize rules term 100))
  ([rules term max-iterations]
   (loop [current term
          iterations 0]
     (if (>= iterations max-iterations)
       current  ; Reached iteration limit
       (let [next-term (simplify-deep rules current)]
         (if (= current next-term)
           current  ; Fixed point reached
           (recur next-term (inc iterations))))))))

(defn theory->normalizer
  "Create a normalization function from a theory's axioms.

  Returns a function that normalizes terms using the theory's axioms.

  Example:
    (def cat-normalize (theory->normalizer Category))
    (cat-normalize my-term)  ;=> normalized term"
  [theory]
  (let [rules (theory->rules theory)]
    (fn [term]
      (normalize rules term))))

;;; ============================================================================
;;; Equational Reasoning
;;; ============================================================================

(defn terms-equal?
  "Check if two terms are equal under the theory's axioms.

  Two terms are equal if they normalize to the same form.
  Uses structural equality (S-expression comparison).

  Args:
    rules - Sequence of rewrite rules (from theory->rules)
    term1 - First AlgTerm
    term2 - Second AlgTerm

  Returns:
    true if terms normalize to the same form, false otherwise"
  [rules term1 term2]
  (let [norm1 (normalize rules term1)
        norm2 (normalize rules term2)
        sexp1 (term->sexp norm1)
        sexp2 (term->sexp norm2)]
    (= sexp1 sexp2)))

(defn normalize-with-trace
  "Normalize a term while recording each rewrite step.

  Returns a vector of terms showing the rewrite sequence from the original
  term to the normal form.

  Args:
    rules - Sequence of rewrite rules
    term - AlgTerm to normalize
    max-iterations - Maximum number of iterations (default: 100)

  Returns:
    Vector of AlgTerms showing each step: [original, step1, step2, ..., normal-form]"
  ([rules term]
   (normalize-with-trace rules term 100))
  ([rules term max-iterations]
   (loop [current term
          trace [term]
          iterations 0]
     (if (>= iterations max-iterations)
       trace  ; Return trace up to iteration limit
       (let [next-term (simplify-deep rules current)]
         (if (= current next-term)
           trace  ; Fixed point reached
           (recur next-term (conj trace next-term) (inc iterations))))))))

(defn rewrite-path
  "Show the rewrite path from term1 to term2 (if they're equal).

  If the terms are equal under the axioms, returns a vector showing
  how to rewrite term1 into term2 through their common normal form.

  Args:
    rules - Sequence of rewrite rules
    term1 - Starting term
    term2 - Target term

  Returns:
    Vector of terms showing: [term1, ..., normal-form, ..., term2]
    or nil if terms are not equal"
  [rules term1 term2]
  (let [trace1 (normalize-with-trace rules term1)
        trace2 (normalize-with-trace rules term2)
        normal1 (last trace1)
        normal2 (last trace2)
        ;; Use S-expression equality for structural comparison
        sexp1 (term->sexp normal1)
        sexp2 (term->sexp normal2)]
    (when (= sexp1 sexp2)
      ;; Terms are equal - show path through normal form
      ;; trace1: term1 -> normal-form
      ;; reverse(trace2): normal-form <- term2
      (vec (concat trace1 (rest (reverse trace2)))))))

(defn prove-equation
  "Try to prove an equation using the theory's axioms.

  Args:
    theory - Theory with axioms
    lhs - Left-hand side term
    rhs - Right-hand side term

  Returns:
    Map with:
      :provable? - true if equation can be proven
      :normal-form - The common normal form (if provable)
      :lhs-steps - Normalization steps for LHS
      :rhs-steps - Normalization steps for RHS"
  [theory lhs rhs]
  (let [rules (theory->rules theory)
        lhs-trace (normalize-with-trace rules lhs)
        rhs-trace (normalize-with-trace rules rhs)
        lhs-normal (last lhs-trace)
        rhs-normal (last rhs-trace)
        ;; Use S-expression equality for structural comparison
        lhs-sexp (term->sexp lhs-normal)
        rhs-sexp (term->sexp rhs-normal)
        equal? (= lhs-sexp rhs-sexp)]
    {:provable? equal?
     :normal-form (when equal? lhs-normal)
     :lhs-steps lhs-trace
     :rhs-steps rhs-trace}))
