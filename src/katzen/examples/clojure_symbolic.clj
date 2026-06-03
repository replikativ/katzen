(ns katzen.examples.clojure-symbolic
  "Symbolic model for ThClojureCore - builds ASTs without evaluation.

  This model constructs abstract syntax trees that can be:
  - Analyzed (free variable analysis, type inference)
  - Transformed (optimization, compilation)
  - Pretty-printed
  - Serialized"
  (:require [katzen.model :as model]
            [katzen.examples.clojure-core :refer [ThClojureCore]]
            [clojure.set :as set]))

(model/defsymbolic SymbolicClojure ThClojureCore
  {:normalize? false})

;; ============================================================================
;; Helper to create AST nodes
;; ============================================================================

(defn make-expr
  "Create an expression AST node"
  [head & args]
  {:head head :args (vec args)})

;; ============================================================================
;; Pretty Printing
;; ============================================================================

(defn pretty-print
  "Convert AST to readable Clojure syntax.

  Takes an expression map with :head and :args, returns a Clojure form
  that can be printed with pr-str or eval'd.

  Examples:
    (pretty-print {:head :lit-num, :args [42]})
    => 42

    (pretty-print {:head :fn-expr, :args [['x] {:head :var, :args ['x]}]})
    => (fn [x] x)"
  [expr]
  (if-not (map? expr)
    expr  ; Already a literal value
    (case (:head expr)
      ;; Literals - just return the value
      :lit-num (first (:args expr))
      :lit-str (first (:args expr))
      :lit-bool (first (:args expr))
      :lit-nil nil

      ;; Variables
      :var (first (:args expr))

      ;; Functions
      :fn-expr (let [[params body] (:args expr)]
                 (list 'fn (vec params) (pretty-print body)))

      ;; Application
      :app (let [[fn-expr arg-exprs] (:args expr)]
             (cons (pretty-print fn-expr)
                   (map pretty-print arg-exprs)))

      ;; Let binding
      :let-expr (let [[name val-expr body] (:args expr)]
                  (list 'let [name (pretty-print val-expr)]
                        (pretty-print body)))

      ;; Letrec
      :letrec-expr (let [[name params fn-body in-expr] (:args expr)]
                     (list 'letrec [name (list 'fn (vec params)
                                               (pretty-print fn-body))]
                           (pretty-print in-expr)))

      ;; If
      :if-expr (let [[test then-expr else-expr] (:args expr)]
                 (list 'if (pretty-print test)
                       (pretty-print then-expr)
                       (pretty-print else-expr)))

      ;; Do
      :do-expr (let [[expr1 expr2] (:args expr)]
                 (list 'do (pretty-print expr1)
                       (pretty-print expr2)))

      ;; Primitives - show as symbols
      :prim-add '+
      :prim-sub '-
      :prim-mul '*
      :prim-div '/
      :prim-eq '=
      :prim-lt '<
      :prim-gt '>

      ;; Data structures
      :lit-vec (let [[elems] (:args expr)]
                 (vec (map pretty-print elems)))

      :lit-map (let [[kvs] (:args expr)]
                 (into {} (map (fn [[k v]]
                                 [(pretty-print k) (pretty-print v)])
                               kvs)))

      :prim-nth 'nth
      :prim-count 'count
      :prim-conj 'conj
      :prim-get 'get
      :prim-assoc 'assoc

      ;; Unknown - show structure
      (list :unknown (:head expr) (map pretty-print (:args expr))))))

(defn pretty-str
  "Pretty-print AST to string"
  [expr]
  (pr-str (pretty-print expr)))

;; ============================================================================
;; Free Variable Analysis
;; ============================================================================

(defn free-vars
  "Return set of free (unbound) variables in an expression.

  Free variables are those that are referenced but not bound by an
  enclosing fn, let, or letrec form.

  Examples:
    (free-vars (make-expr :var 'x))
    => #{'x}

    (free-vars (make-expr :fn-expr ['x] (make-expr :var 'x)))
    => #{}

    (free-vars (make-expr :fn-expr ['x] (make-expr :var 'y)))
    => #{'y}

    (free-vars (make-expr :app (make-expr :var 'f)
                                [(make-expr :var 'x)]))
    => #{'f 'x}"
  [expr]
  (if-not (map? expr)
    #{}  ; Literals have no free vars
    (case (:head expr)
      ;; Literals - no free vars
      (:lit-num :lit-str :lit-bool :lit-nil) #{}

      ;; Variable - it's free
      :var #{(first (:args expr))}

      ;; Function - params bind variables in body
      :fn-expr (let [[params body] (:args expr)]
                 (set/difference (free-vars body) (set params)))

      ;; Application - union of free vars from function and args
      :app (let [[fn-expr arg-exprs] (:args expr)]
             (apply set/union
                    (free-vars fn-expr)
                    (map free-vars arg-exprs)))

      ;; Let - name binds in body, but val-expr evaluated in outer scope
      :let-expr (let [[name val-expr body] (:args expr)]
                  (set/union (free-vars val-expr)
                             (set/difference (free-vars body) #{name})))

      ;; Letrec - name binds in both fn-body and in-expr
      :letrec-expr (let [[name params fn-body in-expr] (:args expr)]
                     (set/difference
                      (set/union
                         ;; Free vars in function body (minus params)
                       (set/difference (free-vars fn-body) (set params))
                         ;; Free vars in in-expr
                       (free-vars in-expr))
                       ;; Minus the recursive name
                      #{name}))

      ;; If - union of all branches
      :if-expr (let [[test then-expr else-expr] (:args expr)]
                 (set/union (free-vars test)
                            (free-vars then-expr)
                            (free-vars else-expr)))

      ;; Do - union of both expressions
      :do-expr (let [[expr1 expr2] (:args expr)]
                 (set/union (free-vars expr1)
                            (free-vars expr2)))

      ;; Primitives - no free vars (they're built-in)
      (:prim-add :prim-sub :prim-mul :prim-div
                 :prim-eq :prim-lt :prim-gt
                 :prim-nth :prim-count :prim-conj
                 :prim-get :prim-assoc) #{}

      ;; Data structures - union of element free vars
      :lit-vec (let [[elems] (:args expr)]
                 (apply set/union #{} (map free-vars elems)))

      :lit-map (let [[kvs] (:args expr)]
                 (apply set/union #{}
                        (map (fn [[k v]]
                               (set/union (free-vars k) (free-vars v)))
                             kvs)))

      ;; Unknown - be conservative, assume no free vars
      #{})))

(defn closed?
  "Check if an expression is closed (has no free variables)"
  [expr]
  (empty? (free-vars expr)))

(defn bound-vars
  "Return set of all variables bound in an expression.

  This includes parameters of functions and names from let/letrec bindings."
  [expr]
  (if-not (map? expr)
    #{}
    (case (:head expr)
      (:lit-num :lit-str :lit-bool :lit-nil :var) #{}

      :fn-expr (let [[params body] (:args expr)]
                 (set/union (set params) (bound-vars body)))

      :app (let [[fn-expr arg-exprs] (:args expr)]
             (apply set/union
                    (bound-vars fn-expr)
                    (map bound-vars arg-exprs)))

      :let-expr (let [[name val-expr body] (:args expr)]
                  (set/union #{name}
                             (bound-vars val-expr)
                             (bound-vars body)))

      :letrec-expr (let [[name params fn-body in-expr] (:args expr)]
                     (set/union #{name}
                                (set params)
                                (bound-vars fn-body)
                                (bound-vars in-expr)))

      :if-expr (let [[test then-expr else-expr] (:args expr)]
                 (set/union (bound-vars test)
                            (bound-vars then-expr)
                            (bound-vars else-expr)))

      :do-expr (let [[expr1 expr2] (:args expr)]
                 (set/union (bound-vars expr1)
                            (bound-vars expr2)))

      (:prim-add :prim-sub :prim-mul :prim-div
                 :prim-eq :prim-lt :prim-gt
                 :prim-nth :prim-count :prim-conj
                 :prim-get :prim-assoc) #{}

      :lit-vec (let [[elems] (:args expr)]
                 (apply set/union #{} (map bound-vars elems)))

      :lit-map (let [[kvs] (:args expr)]
                 (apply set/union #{}
                        (map (fn [[k v]]
                               (set/union (bound-vars k) (bound-vars v)))
                             kvs)))

      #{})))

;; ============================================================================
;; Constant Folding Optimization
;; ============================================================================

(defn literal?
  "Check if an expression is a literal value"
  [expr]
  (and (map? expr)
       (#{:lit-num :lit-str :lit-bool :lit-nil} (:head expr))))

(defn literal-value
  "Extract the value from a literal expression"
  [expr]
  (case (:head expr)
    :lit-num (first (:args expr))
    :lit-str (first (:args expr))
    :lit-bool (first (:args expr))
    :lit-nil nil))

(defn fold-arithmetic
  "Fold arithmetic operations on constants"
  [op args]
  (when (every? literal? args)
    (let [vals (map literal-value args)]
      (when (every? number? vals)
        (case op
          :prim-add (make-expr :lit-num (apply + vals))
          :prim-sub (make-expr :lit-num (apply - vals))
          :prim-mul (make-expr :lit-num (apply * vals))
          :prim-div (when (not-any? zero? (rest vals))
                      (make-expr :lit-num (apply / vals)))
          nil)))))

(defn fold-comparison
  "Fold comparison operations on constants"
  [op args]
  (when (every? literal? args)
    (let [vals (map literal-value args)]
      (when (every? number? vals)
        (case op
          :prim-eq (make-expr :lit-bool (apply = vals))
          :prim-lt (make-expr :lit-bool (apply < vals))
          :prim-gt (make-expr :lit-bool (apply > vals))
          nil)))))

(defn fold-if
  "Fold if expressions with constant test"
  [test then-expr else-expr]
  (when (literal? test)
    (let [test-val (literal-value test)]
      ;; Clojure truthiness: false and nil are falsy, everything else is truthy
      (if (or (false? test-val) (nil? test-val))
        else-expr
        then-expr))))

(defn constant-fold
  "Perform constant folding optimization on an expression.

  This optimization evaluates constant expressions at compile time,
  reducing runtime computation.

  Examples:
    (constant-fold (make-expr :app (make-expr :prim-add)
                                   [(make-expr :lit-num 2)
                                    (make-expr :lit-num 3)]))
    => {:head :lit-num, :args [5]}

    (constant-fold (make-expr :if-expr (make-expr :lit-bool true)
                                        (make-expr :lit-num 1)
                                        (make-expr :lit-num 2)))
    => {:head :lit-num, :args [1]}"
  [expr]
  (if-not (map? expr)
    expr  ; Literals don't need folding
    (case (:head expr)
      ;; Literals - already constant
      (:lit-num :lit-str :lit-bool :lit-nil :var) expr

      ;; Primitives - already constant
      (:prim-add :prim-sub :prim-mul :prim-div
                 :prim-eq :prim-lt :prim-gt
                 :prim-nth :prim-count :prim-conj
                 :prim-get :prim-assoc) expr

      ;; Function - fold body
      :fn-expr (let [[params body] (:args expr)]
                 (make-expr :fn-expr params (constant-fold body)))

      ;; Application - try to fold if applying primitive to constants
      :app (let [[fn-expr arg-exprs] (:args expr)
                 folded-fn (constant-fold fn-expr)
                 folded-args (mapv constant-fold arg-exprs)]
             ;; Try arithmetic folding
             (or (when (#{:prim-add :prim-sub :prim-mul :prim-div} (:head folded-fn))
                   (fold-arithmetic (:head folded-fn) folded-args))
                 ;; Try comparison folding
                 (when (#{:prim-eq :prim-lt :prim-gt} (:head folded-fn))
                   (fold-comparison (:head folded-fn) folded-args))
                 ;; No folding possible, return transformed expression
                 (make-expr :app folded-fn folded-args)))

      ;; Let - fold both val and body
      :let-expr (let [[name val-expr body] (:args expr)
                      folded-val (constant-fold val-expr)
                      folded-body (constant-fold body)]
                  ;; If val is a literal and name is not used, inline it
                  (if (and (literal? folded-val)
                           (not (contains? (free-vars folded-body) name)))
                    ;; Dead binding, just return body
                    folded-body
                    (make-expr :let-expr name folded-val folded-body)))

      ;; Letrec - fold body and in-expr
      :letrec-expr (let [[name params fn-body in-expr] (:args expr)]
                     (make-expr :letrec-expr name params
                                (constant-fold fn-body)
                                (constant-fold in-expr)))

      ;; If - try to fold if test is constant
      :if-expr (let [[test then-expr else-expr] (:args expr)
                     folded-test (constant-fold test)
                     folded-then (constant-fold then-expr)
                     folded-else (constant-fold else-expr)]
                 (or (fold-if folded-test folded-then folded-else)
                     (make-expr :if-expr folded-test folded-then folded-else)))

      ;; Do - fold both expressions
      :do-expr (let [[expr1 expr2] (:args expr)]
                 (make-expr :do-expr
                            (constant-fold expr1)
                            (constant-fold expr2)))

      ;; Data structures - fold elements
      :lit-vec (let [[elems] (:args expr)]
                 (make-expr :lit-vec (mapv constant-fold elems)))

      :lit-map (let [[kvs] (:args expr)]
                 (make-expr :lit-map
                            (mapv (fn [[k v]]
                                    [(constant-fold k) (constant-fold v)])
                                  kvs)))

      ;; Unknown - return as-is
      expr)))

(defn optimize
  "Apply all optimizations to an expression.

  Currently applies:
  - Constant folding
  - Dead code elimination (via constant folding)

  Can be applied multiple times until a fixed point is reached."
  [expr]
  (constant-fold expr))

(defn optimize-until-fixpoint
  "Repeatedly optimize until no more changes occur"
  [expr]
  (let [optimized (optimize expr)]
    (if (= optimized expr)
      expr
      (recur optimized))))
