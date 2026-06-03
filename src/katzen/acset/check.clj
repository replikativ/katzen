(ns katzen.acset.check
  "Runtime axiom checking for ACSet instances.

   Schemas may declare equational axioms in their `:axioms` field
   (see `doc/CONVENTIONS.md` for the shape). `check-axioms!` walks
   every context binding, evaluates the axiom's LHS and RHS against
   the instance, and reports the first violation — the operational
   guarantee that Catlab.jl's `is_natural`, the chase, and `@instance`
   together approximate but none in full.

   Axiom expressions are tiny s-expressions in three flavors:

     - a symbol bound by `:ctx`        → look up its current value
     - a list `(hom arg)`              → apply hom to the recursively-
                                          evaluated argument via subpart
     - any other value                  → constant (numbers, keywords, …)

   Symbols in head position are translated to keywords for the
   subpart lookup, so users can write `(inv (inv e))` even though the
   schema stores the hom under `:inv`.

   When any sub-evaluation is `nil` (a partial morphism — some part
   doesn't have that value assigned yet), the axiom is treated as
   vacuously satisfied at that binding. This matches the existing
   partial-morphism semantics in `katzen.acset.migration` and avoids
   spurious failures from in-progress data."
  (:require [katzen.acset :as a]))

;; ============================================================================
;; Term evaluation
;; ============================================================================

(defn- as-keyword
  "Symbol → keyword in head position."
  [x]
  (if (symbol? x) (keyword (name x)) x))

(defn- eval-term
  "Evaluate a term expression `t` against `acset` with `bindings` from
   ctx-variable symbols to part-ids. Returns the resulting part-id /
   value, or `nil` if any subpart on the path is unset."
  [acset bindings t]
  (cond
    (symbol? t)
    (or (get bindings t)
        (throw (ex-info "Unbound symbol in axiom"
                        {:symbol t :known (keys bindings)})))

    (seq? t)
    (let [[head & args] t
          mname (as-keyword head)]
      (when-not (= 1 (count args))
        (throw (ex-info "Only unary hom/attr applications supported"
                        {:term t})))
      (let [arg-val (eval-term acset bindings (first args))]
        (when (some? arg-val)
          (a/subpart acset mname arg-val))))

    :else t))

;; ============================================================================
;; Context binding enumeration
;; ============================================================================

(defn- all-bindings
  "Cross-product of {:name … :type …} entries in `ctx` against the
   instance's parts. Returns a lazy seq of {var-sym → part-id} maps."
  [acset ctx]
  (reduce
   (fn [acc {var-name :name var-type :type}]
     (for [partial acc
           part-id (a/parts acset var-type)]
       (assoc partial var-name part-id)))
   [{}]
   ctx))

;; ============================================================================
;; Public API
;; ============================================================================

(defn check-axiom
  "Verify a single axiom on `acset`. Returns nil on full pass; on the
   first failure returns a map describing the violation:
     {:axiom axiom-name :bindings {…} :lhs-eval … :rhs-eval …}"
  [acset {axiom-name :name ctx :ctx lhs :lhs rhs :rhs}]
  (some
   (fn [bindings]
     (let [lhs-val (eval-term acset bindings lhs)
           rhs-val (eval-term acset bindings rhs)]
       ;; A nil on either side means a partial morphism; treat as vacuous.
       (cond
         (or (nil? lhs-val) (nil? rhs-val)) nil
         (= lhs-val rhs-val) nil
         :else {:axiom axiom-name
                :bindings bindings
                :lhs-eval lhs-val
                :rhs-eval rhs-val})))
   (all-bindings acset ctx)))

(defn check-axioms
  "Walk every axiom on `(a/schema acset)`. Returns nil if all pass, or
   the first violation."
  [acset]
  (some #(check-axiom acset %) (:axioms (a/schema acset))))

(defn check-axioms!
  "Strict variant: returns `acset` on success, throws on the first
   violation. Use at trust boundaries (after `migrate`, before
   `compile-rhs`, on imported instances) to guarantee axiomatic
   consistency."
  [acset]
  (when-let [v (check-axioms acset)]
    (throw (ex-info (str "ACSet axiom violation: " (:axiom v))
                    v)))
  acset)
