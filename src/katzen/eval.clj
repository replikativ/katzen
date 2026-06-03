(ns katzen.eval
  "A general term evaluator over an ACSet + a type-side MODEL — the engine behind
   COMPUTED properties and VALIDATION predicates (and, with katzen.aggregate,
   rollups). This is the type-side realized: a GAT's operations as host functions.

   A term is an s-expression:
     - a symbol         → looked up in `bindings` (a context variable)
     - (head arg …)     → if `head` names a HOM/ATTR of the schema and there is
                          exactly one arg, NAVIGATE the ACSet (subpart) — `head`
                          may be written as a bare symbol (translated to a
                          keyword); otherwise `head` is a type-side OPERATION
                          resolved in `model` and applied to the evaluated args
     - any other value  → a constant (number, string, keyword, …)

   `model` maps an operation symbol → a host fn (the computational model of the
   type-side). `base-model` covers common arithmetic / string / comparison /
   predicate ops; merge onto it to extend. Morphism navigation propagates nil
   (a partial morphism / unset value), matching katzen.acset.check; type-side
   operations are applied as-is (the term author handles nils, e.g. via `nil?`)."
  (:require [katzen.acset :as a]))

(def base-model
  "Common type-side operations as host fns. Merge a map onto this to add
   domain operations (e.g. regex validators, custom formulas)."
  {'+ + '- - '* * '/ / 'inc inc 'dec dec 'mod mod 'quot quot 'rem rem
   'max max 'min min 'abs abs 'count count
   'str str 'name name 'keyword keyword 'subs subs 'clojure.string/upper-case clojure.string/upper-case
   '= = 'not= not= '< < '> > '<= <= '>= >= 'not not
   'and (fn [& xs] (every? identity xs)) 'or (fn [& xs] (boolean (some identity xs)))
   'pos? pos? 'neg? neg? 'zero? zero? 'nil? nil? 'some? some? 'empty? empty?
   'boolean boolean 're-matches re-matches 're-find re-find 'contains? contains? 'get get})

(defn eval-term
  "Evaluate term `t` against `acset` with operations from `model` and context
   `bindings` (symbol → value). See ns doc for the term grammar."
  [acset model bindings t]
  (cond
    (symbol? t)
    (if (contains? bindings t)
      (get bindings t)
      (throw (ex-info "Unbound symbol in term" {:symbol t :known (vec (keys bindings))})))

    (seq? t)
    (let [[head & args] t
          mname (keyword (name head))]
      (cond
        (and (= 1 (count args)) (a/morphism-by-name (a/schema acset) mname))
        (let [v (eval-term acset model bindings (first args))]
          (when (some? v) (a/subpart acset mname v)))

        (contains? model head)
        (apply (get model head) (map #(eval-term acset model bindings %) args))

        :else
        (throw (ex-info "Unknown operation or morphism in term"
                        {:head head :known-ops (vec (keys model))}))))

    :else t))

;; ---------------------------------------------------------------------------
;; Computed properties (derived attributes) and validation predicates

(defn derived
  "Evaluate a COMPUTED property at `part`: a term over a single context variable
   (`:var`, default `x`) bound to the part. `prop` = {:var <sym?> :term <expr>}.
   The term lives in the schema layer; this evaluates it in the model — the
   categorical 'computed attribute = a term-in-context.'"
  ([acset prop part] (derived acset base-model prop part))
  ([acset model {:keys [var term] :or {var 'x}} part]
   (eval-term acset model {var part} term)))

(defn derived-all
  "Map every part of `:dom` to its derived value: a materialized view of the
   computed property over the whole object."
  ([acset prop] (derived-all acset base-model prop))
  ([acset model {:keys [dom] :as prop}]
   (into {} (for [p (a/parts acset dom)] [p (derived acset model prop p)]))))

(defn invalid
  "Parts of `:dom` where the Bool-valued predicate `:pred` (a term over `:var`,
   default `x`) is falsey — a VALIDATION as a type-side predicate. Returns a seq
   of `{:part p :value <pred result>}`. `valid?` is the boolean summary."
  ([acset prop] (invalid acset base-model prop))
  ([acset model {:keys [dom var pred] :or {var 'x}}]
   (for [p (a/parts acset dom)
         :let [v (eval-term acset model {var p} pred)]
         :when (not v)]
     {:part p :value v})))

(defn valid?
  ([acset prop] (empty? (invalid acset prop)))
  ([acset model prop] (empty? (invalid acset model prop))))
