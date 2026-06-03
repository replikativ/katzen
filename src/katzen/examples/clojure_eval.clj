(ns katzen.examples.clojure-eval
  "Standard evaluation semantics for ThClojureCore.

  This model implements call-by-value (eager) evaluation for the minimal
  Clojure kernel. It interprets expressions as a simple tree-walking evaluator.

  Values are represented as Clojure data:
  - Numbers, strings, booleans, nil: themselves
  - Functions: {:type :closure, :param sym, :body expr, :env env}
  - Primitives: {:type :primitive, :fn clojure-fn}

  Environments are Clojure maps: symbol → value"
  (:require [katzen.model :as model]
            [katzen.examples.clojure-core :refer [ThClojureCore]]))

(defn truthy?
  "Determine if a value is truthy (Clojure semantics)"
  [v]
  (not (or (false? v) (nil? v))))

(defn make-closure
  "Create a closure value (multi-arity)"
  [params body env]
  {:type :closure
   :params params  ; Now a list of params
   :body body
   :env env})

(defn make-primitive
  "Create a primitive function value"
  [f]
  {:type :primitive
   :fn f})

(defn apply-value
  "Apply a value (closure or primitive) to arguments (multi-arity)"
  [fn-val arg-vals]
  (case (:type fn-val)
    :closure
    (let [{:keys [params body env]} fn-val]
      ;; Check arity
      (if (= (count params) (count arg-vals))
        {:eval-needed true
         :body body
         :env (merge env (zipmap params arg-vals))}
        (throw (ex-info "Arity mismatch"
                       {:expected (count params)
                        :got (count arg-vals)
                        :params params
                        :args arg-vals}))))

    :primitive
    {:eval-needed false
     :value (apply (:fn fn-val) arg-vals)}  ; Use apply for varargs

    (throw (ex-info "Cannot apply non-function" {:fn-val fn-val}))))

;; Forward declaration for mutual recursion
(declare eval-expr)

(model/definstance StandardEval ThClojureCore
  "Call-by-value evaluation model for Clojure core.

  This model evaluates expressions to values. Expressions are represented as
  maps with :head (term name) and :args (arguments)."

  {:expr-type :ast-node
   :value-type :clojure-value
   :env-type :hash-map}

  ;; Type predicates (all permissive for now)
  (Expr [_model args] true)
  (Value [_model args] true)
  (Env [_model args]
    (let [[env] args]
      (or (nil? env) (map? env))))

  ;; Literals - just return the wrapped value
  (lit-num [_model args]
    (let [[n] args] n))

  (lit-str [_model args]
    (let [[s] args] s))

  (lit-bool [_model args]
    (let [[b] args] b))

  (lit-nil [_model args]
    nil)

  ;; Variables - lookup in environment
  (var [_model args]
    (let [[name env] args]
      (if (contains? env name)
        (get env name)
        (throw (ex-info "Unbound variable" {:var name :env env})))))

  ;; Functions - create closure (multi-arity)
  (fn-expr [_model args]
    (let [[params body env] args]
      (make-closure params body env)))

  ;; Application - evaluate function and arguments, then apply (multi-arity)
  (app [model args]
    (let [[fn-expr arg-exprs env] args
          ;; Evaluate function and arguments
          fn-val (eval-expr model fn-expr env)
          arg-vals (mapv #(eval-expr model % env) arg-exprs)
          ;; Apply
          result (apply-value fn-val arg-vals)]
      (if (:eval-needed result)
        ;; Need to evaluate body in extended env
        (eval-expr model (:body result) (:env result))
        ;; Primitive already computed result
        (:value result))))

  ;; Let - evaluate binding value, extend env, evaluate body
  (let-expr [model args]
    (let [[name val-expr body env] args
          val (eval-expr model val-expr env)
          new-env (assoc env name val)]
      (eval-expr model body new-env)))

  ;; Letrec - recursive let binding
  (letrec-expr [model args]
    (let [[name params fn-body in-expr env] args
          ;; Create a placeholder for the recursive reference
          rec-closure (atom nil)
          ;; Create closure with environment that will contain itself
          closure {:type :closure
                   :params params
                   :body fn-body
                   :env (assoc env name rec-closure)}]
      ;; Fill in the recursive reference
      (reset! rec-closure closure)
      ;; Evaluate the in-expr with the recursive function bound
      (eval-expr model in-expr (assoc env name closure))))

  ;; If - evaluate test, then branch based on truthiness
  (if-expr [model args]
    (let [[test then-expr else-expr env] args
          test-val (eval-expr model test env)]
      (if (truthy? test-val)
        (eval-expr model then-expr env)
        (eval-expr model else-expr env))))

  ;; Do - evaluate expr1 for side effects, return expr2
  (do-expr [model args]
    (let [[expr1 expr2 env] args]
      (eval-expr model expr1 env)  ; Evaluate but discard
      (eval-expr model expr2 env)))

  ;; Primitives - return primitive values
  (prim-add [_model args]
    (make-primitive +))

  (prim-sub [_model args]
    (make-primitive -))

  (prim-mul [_model args]
    (make-primitive *))

  (prim-div [_model args]
    (make-primitive /))

  (prim-eq [_model args]
    (make-primitive =))

  (prim-lt [_model args]
    (make-primitive <))

  (prim-gt [_model args]
    (make-primitive >))

  ;; Data structure literals
  (lit-vec [model args]
    (let [[elems env] args]
      (vec (map #(eval-expr model % env) elems))))

  (lit-map [model args]
    (let [[kvs env] args]
      (into {} (map (fn [[k v]]
                      [(eval-expr model k env)
                       (eval-expr model v env)])
                    kvs))))

  ;; Data structure operations
  (prim-nth [_model args]
    (make-primitive nth))

  (prim-count [_model args]
    (make-primitive count))

  (prim-conj [_model args]
    (make-primitive conj))

  (prim-get [_model args]
    (make-primitive get))

  (prim-assoc [_model args]
    (make-primitive assoc))

  ;; Environment operations
  (empty-env [_model args]
    {})

  (extend-env [_model args]
    (let [[env name val] args]
      (assoc env name val)))

  (lookup-env [_model args]
    (let [[env name] args]
      (get env name))))

;; Helper function to evaluate expressions
;; This dispatches based on the expression's :head field
(defn eval-expr
  "Evaluate an expression in an environment using the StandardEval model.

  expr: An expression map with :head and :args
  env: Environment map (symbol → value)

  Returns: The evaluated value"
  [model expr env]
  (let [head (:head expr)
        args (:args expr)]
    (case head
      ;; Literals
      :lit-num (first args)
      :lit-str (first args)
      :lit-bool (first args)
      :lit-nil nil

      ;; Variable lookup
      :var (let [[name] args]
             (if (contains? env name)
               (let [val (get env name)]
                 ;; Dereference atoms (for recursive closures)
                 (if (instance? clojure.lang.Atom val)
                   @val
                   val))
               (throw (ex-info "Unbound variable" {:var name}))))

      ;; Function (multi-arity)
      :fn-expr (let [[params body] args]
                 (make-closure params body env))

      ;; Application (multi-arity)
      :app (let [[fn-expr arg-exprs] args
                 fn-val (eval-expr model fn-expr env)
                 arg-vals (mapv #(eval-expr model % env) arg-exprs)
                 result (apply-value fn-val arg-vals)]
             (if (:eval-needed result)
               (eval-expr model (:body result) (:env result))
               (:value result)))

      ;; Let
      :let-expr (let [[name val-expr body] args
                      val (eval-expr model val-expr env)
                      new-env (assoc env name val)]
                  (eval-expr model body new-env))

      ;; Letrec
      :letrec-expr (let [[name params fn-body in-expr] args
                         rec-closure (atom nil)
                         closure {:type :closure
                                  :params params
                                  :body fn-body
                                  :env (assoc env name rec-closure)}]
                     (reset! rec-closure closure)
                     (eval-expr model in-expr (assoc env name closure)))

      ;; If
      :if-expr (let [[test then-expr else-expr] args
                     test-val (eval-expr model test env)]
                 (if (truthy? test-val)
                   (eval-expr model then-expr env)
                   (eval-expr model else-expr env)))

      ;; Do
      :do-expr (let [[expr1 expr2] args]
                 (eval-expr model expr1 env)
                 (eval-expr model expr2 env))

      ;; Primitives
      :prim-add (make-primitive +)
      :prim-sub (make-primitive -)
      :prim-mul (make-primitive *)
      :prim-div (make-primitive /)
      :prim-eq (make-primitive =)
      :prim-lt (make-primitive <)
      :prim-gt (make-primitive >)

      ;; Data structure literals
      :lit-vec (let [[elems] args]
                 (vec (map #(eval-expr model % env) elems)))

      :lit-map (let [[kvs] args]
                 (into {} (map (fn [[k v]]
                                 [(eval-expr model k env)
                                  (eval-expr model v env)])
                               kvs)))

      ;; Data structure operations
      :prim-nth (make-primitive nth)
      :prim-count (make-primitive count)
      :prim-conj (make-primitive conj)
      :prim-get (make-primitive get)
      :prim-assoc (make-primitive assoc)

      ;; Unknown
      (throw (ex-info "Unknown expression type" {:head head :args args})))))
