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
   spurious failures from in-progress data.

   PATH EQUATIONS. The general `:axioms` form above is expressive but verbose
   for the common case — two morphism PATHS out of one object that must agree
   (the ACSets.jl `eqs` idiom, `Eq = (name, dom, codom, (path1, path2))`).
   Such equations may instead be declared in a schema's `:equations` field as

     {:name <opt> :dom <Ob> :lhs [m1 m2 …] :rhs [n1 …] :codom <opt>}

   where `:lhs`/`:rhs` are sequences of hom/attr names applied LEFT-TO-RIGHT
   (an empty path is the identity). `path-equation->axiom` compiles each into
   the general axiom shape, and `check-axioms`/`check-axioms!` check `:equations`
   and `:axioms` together — so path equations reuse exactly this checker."
  (:require [clojure.string :as str]
            [katzen.acset :as a]))

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
;; Path equations (the ACSets.jl `eqs` idiom) → general axioms
;; ============================================================================

(defn- path->term
  "Compile a path (seq of hom/attr names, applied LEFT-TO-RIGHT) into an axiom
   term over the variable `var-sym`. The empty path is the identity (the
   variable itself); `[:f :g]` becomes `(g (f x))`."
  [var-sym path]
  (reduce (fn [acc m] (list (symbol (name m)) acc)) var-sym path))

(defn path-equation->axiom
  "Compile an ACSets.jl-style path equation

     {:name <opt> :dom <Ob> :lhs [m …] :rhs [n …] :codom <opt>}

   (two morphism paths out of object `:dom` that must agree on every part)
   into the general axiom shape consumed by `check-axiom`. `:lhs`/`:rhs` are
   sequences of hom/attr names applied left-to-right; an empty path is the
   identity. For richer (non-path) term equations, write an `:axioms` entry
   directly instead."
  [{:keys [name dom lhs rhs]}]
  (assert dom ":dom is required for a path equation")
  {:name (or name (symbol (str (str/join "," (map clojure.core/name lhs))
                               "=" (str/join "," (map clojure.core/name rhs)))))
   :ctx  [{:name 'x :type dom}]
   :lhs  (path->term 'x lhs)
   :rhs  (path->term 'x rhs)})

(defn schema-axioms
  "Every checkable axiom of `schema`: its explicit `:axioms`, plus the path
   equations in `:equations` compiled via `path-equation->axiom`."
  [schema]
  (concat (:axioms schema)
          (map path-equation->axiom (:equations schema))))

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
  "Walk every axiom on `(a/schema acset)` — both the explicit `:axioms` and the
   path equations in `:equations` (see `schema-axioms`). Returns nil if all pass, or
   the first violation."
  [acset]
  (some #(check-axiom acset %) (schema-axioms (a/schema acset))))

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
