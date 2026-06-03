(ns katzen.symbolic.normalize
  "Directed-rewrite normalization for `katzen.core/AlgTerm` — Layer 2 of
   the symbolic-reasoning story.

   Same shape as `katzen.acset.normalize` (size-decrease orientation,
   bottom-up rewriting, fixed-point loop) but lifted onto the typed
   GAT term layer instead of s-expressions:

     (normalize gat term)        — canonical form under the theory's axioms
     (equiv?    gat t1 t2)       — `(= (normalize t1) (normalize t2))`

   Why both layers: schema axioms live as s-expressions (concrete data
   for the ACSet layer); GAT axioms live as typed AlgTerms with Idents
   carrying scope info. The pattern-matching logic differs in how it
   recognizes a 'variable' (membership in `(:idents ctx)`) and in how
   it walks (over `:args` rather than `rest`), but the rule-derivation
   and fixed-point loop are the same.

   Layer 2 use case: when applying a theory morphism via
   `katzen.morphism/pushforward-term`, the resulting term often
   contains immediate redexes the morphism introduced (e.g. a unit
   inserted by an inclusion). Calling `normalize` on the pushforward
   gives Catlab's `FreeCategory`-style canonical output without
   touching the morphism machinery."
  (:require [katzen.core :as core]
            [katzen.scope :as scope]))

;; ============================================================================
;; AlgTerm structural helpers
;; ============================================================================

;;
;; Two arg shapes appear inside AlgTerms:
;;   - a bare `Ident` in :args means a *context-variable reference*;
;;   - an `AlgTerm` in :args means a *constructor application* (sub-term).
;; Top-level term values are always AlgTerm. At the leaves a var-ref
;; appears either bare (in arg position) or wrapped as an AlgTerm with
;; empty :args (as the entire RHS or LHS of an axiom). The matcher
;; handles both.

(defn- var-ref?
  "True iff `node` is a ctx-var reference (bare Ident in `vars`, OR an
   empty-arg AlgTerm whose head is in `vars`)."
  [vars node]
  (or (and (scope/gat-ident? node) (contains? vars node))
      (and (core/alg-term? node)
           (empty? (:args node))
           (contains? vars (:head node)))))

(defn- var-ref-ident
  "Pull the var Ident out of a var-ref (bare or AlgTerm-leaf)."
  [node]
  (if (scope/gat-ident? node) node (:head node)))

(defn term-equal?
  "Equality modulo the AlgTerm-leaf vs bare-Ident representational quirk.

   The katzen AlgTerm syntax represents a var-reference EITHER as a
   bare Ident (in arg position) OR as an AlgTerm with empty :args (as
   the entire RHS/LHS of an axiom, or top-level). Direct `=` between
   the two shapes returns false even though semantically they denote
   the same variable. This predicate treats them as equal."
  [a b]
  (cond
    (and (scope/gat-ident? a) (scope/gat-ident? b))
    (= a b)

    (and (scope/gat-ident? a)
         (core/alg-term? b) (empty? (:args b)))
    (= a (:head b))

    (and (core/alg-term? a) (empty? (:args a))
         (scope/gat-ident? b))
    (= (:head a) b)

    (and (core/alg-term? a) (core/alg-term? b))
    (and (= (:head a) (:head b))
         (= (count (:args a)) (count (:args b)))
         (every? (fn [[x y]] (term-equal? x y))
                 (map vector (:args a) (:args b))))

    :else (= a b)))

(defn- term-size
  "Number of constructor applications + variable references in a term."
  [t]
  (cond
    (core/alg-term? t) (apply + 1 (map term-size (:args t)))
    :else 1))

;; ============================================================================
;; Pattern matching with ident capture
;; ============================================================================

(defn- match
  "Match `pattern` against `target`. Idents in `vars` are wildcards;
   repeated occurrences must agree. Returns bindings on success,
   nil on failure. Both pattern and target may be bare Idents or
   AlgTerms (see arg-shape note above)."
  ([vars pattern target] (match vars pattern target {}))
  ([vars pattern target bindings]
   (cond
     ;; Var-reference in the pattern → capture the whole target.
     (var-ref? vars pattern)
     (let [v (var-ref-ident pattern)]
       (if (contains? bindings v)
         (when (term-equal? target (get bindings v)) bindings)
         (assoc bindings v target)))

     ;; Non-var bare Ident → match only the same Ident.
     (scope/gat-ident? pattern)
     (when (= pattern target) bindings)

     ;; AlgTerm constructor application → heads + arity must agree, then recurse.
     (and (core/alg-term? pattern) (core/alg-term? target)
          (= (:head pattern) (:head target))
          (= (count (:args pattern)) (count (:args target))))
     (reduce
      (fn [acc [p t]]
        (when acc (match vars p t acc)))
      bindings
      (map vector (:args pattern) (:args target)))

     :else nil)))

(defn- substitute
  "Instantiate `pattern` by replacing each var-reference with its
   bound value. Bindings values may themselves be bare Idents or
   AlgTerms; we splice them in unchanged."
  [bindings pattern]
  (cond
    ;; Bare ident — direct lookup.
    (scope/gat-ident? pattern)
    (get bindings pattern pattern)

    ;; AlgTerm leaf var-reference — replace with the binding.
    (and (core/alg-term? pattern)
         (empty? (:args pattern))
         (contains? bindings (:head pattern)))
    (get bindings (:head pattern))

    ;; AlgTerm constructor application — recurse on args.
    (core/alg-term? pattern)
    (core/alg-term (:head pattern)
                   (mapv #(substitute bindings %) (:args pattern))
                   (:type pattern))

    :else pattern))

;; ============================================================================
;; Axiom orientation
;; ============================================================================

(defn- ctx-var-set
  "Set of Idents bound in an axiom's ctx — the wildcards during matching."
  [axiom]
  (set (:idents (:ctx axiom))))

(defn orient-axiom
  "Convert an AlgAxiom into a directed rewrite rule. See
   `katzen.acset.normalize/orient-axiom` for the orientation algorithm —
   identical logic, lifted onto AlgTerms.

   Axioms can carry an optional `:canonical` field (added via
   `(assoc ax :canonical :lhs)`) declaring which side is canonical;
   that side wins over the size-based default."
  [axiom]
  (let [{lhs :lhs rhs :rhs canonical :canonical} axiom
        ls (term-size lhs)
        rs (term-size rhs)]
    (cond
      (= :lhs canonical)
      {:vars (ctx-var-set axiom) :lhs rhs :rhs lhs
       :name (:name axiom) :origin :canonical-lhs}

      (= :rhs canonical)
      {:vars (ctx-var-set axiom) :lhs lhs :rhs rhs
       :name (:name axiom) :origin :canonical-rhs}

      (> ls rs)
      {:vars (ctx-var-set axiom) :lhs lhs :rhs rhs
       :name (:name axiom) :origin :axiom}

      (> rs ls)
      {:vars (ctx-var-set axiom) :lhs rhs :rhs lhs
       :name (:name axiom) :origin :axiom-flipped}

      :else nil)))

(defn theory-rules
  "Derive every orientable rule from a GAT's axioms."
  [gat]
  (vec (keep orient-axiom (:axioms gat))))

(defn axiom-orientation-report
  "Diagnostic for which of a GAT's axioms become rules and which don't."
  [gat]
  (let [axioms (:axioms gat)
        orientable    (filter orient-axiom axioms)
        non-orientable (remove orient-axiom axioms)]
    {:total (count axioms)
     :orientable (count orientable)
     :non-orientable (mapv :name non-orientable)}))

;; ============================================================================
;; Rule application + bottom-up walk
;; ============================================================================

(defn- try-rule
  "Apply `rule` at the top of `term`. Returns the rewritten term or nil."
  [rule term]
  (when-let [bindings (match (:vars rule) (:lhs rule) term)]
    (substitute bindings (:rhs rule))))

(defn- apply-rules-once
  [rules term]
  (some #(try-rule % term) rules))

(defn- normalize-step
  "Walk `term` bottom-up: normalize :args first, then try the rules at
   the current node. One step (no fixed-point loop)."
  [rules term]
  (let [t' (if (core/alg-term? term)
             (core/alg-term (:head term)
                            (mapv #(normalize-step rules %) (:args term))
                            (:type term))
             term)]
    (or (apply-rules-once rules t')
        t')))

(defn normalize
  "Apply the GAT's orientable axioms as directed rewrites until fixed
   point. Returns the canonical form of `term`."
  [gat term]
  (let [rules (theory-rules gat)]
    (loop [t term, guard 1000]
      (when (zero? guard)
        (throw (ex-info "normalize: rewrite did not reach fixed point in 1000 steps"
                        {:theory (:name gat) :last t})))
      (let [t' (normalize-step rules t)]
        (if (term-equal? t t') t (recur t' (dec guard)))))))

;; ============================================================================
;; Equivalence
;; ============================================================================

(defn equiv?
  "Two AlgTerms are equivalent under the GAT's orientable axioms iff
   their normal forms are structurally equal (modulo the
   AlgTerm-leaf vs bare-Ident representational quirk; see
   `term-equal?`)."
  [gat t1 t2]
  (term-equal? (normalize gat t1) (normalize gat t2)))
