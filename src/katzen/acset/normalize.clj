(ns katzen.acset.normalize
  "Directed-rewrite normalization for schema-axiom-style s-expressions.

   The Julia stack (GATlab.jl + Catlab.jl) ships ~104 LOC of directed
   rewrite combinators (`GATExprUtils.jl`: `associate_unit`, `involute`,
   `distribute_unary`, …) wired into the smart constructors of every
   `@symbolic_model`. This namespace mirrors that pattern for our
   `:axioms` data: try to *orient* each axiom as a size-decreasing
   rewrite, then walk a term bottom-up to fixed point.

   Why size-decreasing: a rewrite that shrinks the term is guaranteed
   to terminate. Involution `(f (f x)) = x`, unit elimination
   `(op u x) = x`, and `(src (inv e)) = (tgt e)` all fit this shape.
   Associativity does not (both sides are the same size) — for v1 we
   leave non-orientable axioms as inert and document the limitation.
   A later pass can add per-shape orient hints (`:assoc-left`, etc.)
   or punt to an e-graph.

   The API is intentionally close to Catlab's:

     (normalize schema term)        — bottom-up rewrite to canonical form
     (equiv?    schema t1 t2)       — `(= (normalize t1) (normalize t2))`
     (theory-rules schema)          — inspect what rules were derived

   This is Layer 1 of the symbolic-reasoning story. Layer 2
   (`katzen.symbolic.normalize`) lifts the same machinery onto
   AlgTerms in the katzen.theory layer.")

;; ============================================================================
;; Term-counting + simple pattern matching
;; ============================================================================

(defn- node-count
  "Number of nodes in a term tree (leaves count as 1)."
  [t]
  (if (seq? t)
    (apply + 1 (map node-count (rest t)))
    1))

(defn- match
  "Try to match `pattern` against `term`. Symbols in `vars` are wildcards
   that capture; same-name multiple occurrences must agree. Returns a
   bindings map on success, nil on failure.

   - vars ∋ pattern    → bind pattern→term (if compatible with bindings)
   - pattern leaf      → match iff equal
   - pattern list      → match heads + recurse on args"
  ([vars pattern term]
   (match vars pattern term {}))
  ([vars pattern term bindings]
   (cond
     (contains? vars pattern)
     (if (contains? bindings pattern)
       (when (= term (get bindings pattern)) bindings)
       (assoc bindings pattern term))

     (and (seq? pattern) (seq? term))
     (when (and (= (count pattern) (count term))
                (= (first pattern) (first term)))
       (reduce
        (fn [acc [p t]]
          (when acc (match vars p t acc)))
        bindings
        (map vector (rest pattern) (rest term))))

     :else
     (when (= pattern term) bindings))))

(defn- substitute
  "Replace each `vars`-symbol occurrence in `pattern` with its bound value."
  [bindings pattern]
  (cond
    (contains? bindings pattern) (get bindings pattern)
    (seq? pattern) (cons (first pattern)
                         (mapv #(substitute bindings %) (rest pattern)))
    :else pattern))

;; ============================================================================
;; Axiom orientation
;; ============================================================================

(defn- ctx-vars
  "Set of variable symbols bound by an axiom's :ctx."
  [axiom]
  (set (map :name (:ctx axiom))))

(defn orient-axiom
  "Convert an axiom to a directed rewrite rule.

   Returns either
     {:vars #{…} :lhs <pattern> :rhs <substitution> :name … :origin <tag>}
   or `nil` when no orientation is available.

   Two orientation modes, in priority order:

   1. **Explicit canonical hint.** If the axiom carries `:canonical :lhs`
      or `:canonical :rhs`, that side is declared the canonical form and
      the rule rewrites the other side into it. Use this for axioms that
      aren't size-decreasing — associativity is the canonical example.

   2. **Size-decrease.** Otherwise, if one side is strictly larger by
      node-count, orient larger → smaller. Same-size axioms with no
      hint are non-orientable and return nil."
  [axiom]
  (let [{lhs :lhs rhs :rhs canonical :canonical} axiom
        lhs-size (node-count lhs)
        rhs-size (node-count rhs)]
    (cond
      (= :lhs canonical)
      {:vars (ctx-vars axiom) :lhs rhs :rhs lhs
       :name (:name axiom) :origin :canonical-lhs}

      (= :rhs canonical)
      {:vars (ctx-vars axiom) :lhs lhs :rhs rhs
       :name (:name axiom) :origin :canonical-rhs}

      (> lhs-size rhs-size)
      {:vars (ctx-vars axiom) :lhs lhs :rhs rhs
       :name (:name axiom) :origin :axiom}

      (> rhs-size lhs-size)
      {:vars (ctx-vars axiom) :lhs rhs :rhs lhs
       :name (:name axiom) :origin :axiom-flipped}

      :else nil)))

(defn theory-rules
  "All orientable rules derivable from a schema's `:axioms`. Inert
   (same-size) axioms drop out; the count of orientable vs total is
   useful for sanity-checking what got picked up."
  [schema]
  (vec (keep orient-axiom (:axioms schema))))

(defn axiom-orientation-report
  "{:total <int> :orientable <int> :non-orientable [<axiom-name>…]}
   — for diagnosing which axioms a schema's normalizer will and won't act on."
  [schema]
  (let [axioms (:axioms schema)
        orientable    (filter orient-axiom axioms)
        non-orientable (remove orient-axiom axioms)]
    {:total (count axioms)
     :orientable (count orientable)
     :non-orientable (mapv :name non-orientable)}))

;; ============================================================================
;; Rule application + bottom-up normalize
;; ============================================================================

(defn- try-rule
  "Return the rewritten term if `rule` matches at the top of `term`,
   otherwise nil."
  [rule term]
  (when-let [bindings (match (:vars rule) (:lhs rule) term)]
    (substitute bindings (:rhs rule))))

(defn- apply-rules-once
  "Try each rule in order; return the first rewrite that fires, or nil."
  [rules term]
  (some #(try-rule % term) rules))

(defn- normalize-step
  "Walk `term` bottom-up: normalize children first, then try to apply
   any rule at the current node. Returns the (one-step) updated term."
  [rules term]
  (let [t' (if (seq? term)
             (cons (first term) (mapv #(normalize-step rules %) (rest term)))
             term)]
    (or (apply-rules-once rules t')
        t')))

(defn normalize
  "Apply the schema's orientable axioms as directed rewrites until
   fixed point. Returns the canonical form of `term`. Bottom-up so
   that rewrites at deeper levels don't have to re-walk."
  [schema term]
  (let [rules (theory-rules schema)]
    (loop [t term, guard 1000]
      (when (zero? guard)
        (throw (ex-info "normalize: rewrite did not reach fixed point in 1000 steps"
                        {:schema (:name schema) :last t})))
      (let [t' (normalize-step rules t)]
        (if (= t t') t (recur t' (dec guard)))))))

;; ============================================================================
;; Equivalence
;; ============================================================================

(defn equiv?
  "Two terms are equivalent under the schema's orientable axioms iff
   their normal forms are syntactically equal. Note: this is sound
   for the orientable subset only — if the schema has non-orientable
   axioms (e.g. associativity) some genuine equalities will be missed."
  [schema t1 t2]
  (= (normalize schema t1) (normalize schema t2)))
