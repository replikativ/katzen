(ns katzen.examples.clojure-core-test
  "Tests for the Clojure kernel GAT encoding.

  This demonstrates how GATs can encode programming language semantics."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.core :as gat-core]
            [katzen.examples.clojure-core :as clj-core]
            [katzen.examples.clojure-eval :as clj-eval]
            [katzen.examples.clojure-symbolic :as clj-sym]))

;; Helper to create AST nodes
(defn make-expr
  "Create an expression AST node"
  [head & args]
  {:head head :args (vec args)})

(deftest test-literals
  (testing "Literal evaluation"
    (let [model (clj-eval/->StandardEval)
          env {}]
      (is (= 42
             (clj-eval/eval-expr model (make-expr :lit-num 42) env)))
      (is (= "hello"
             (clj-eval/eval-expr model (make-expr :lit-str "hello") env)))
      (is (= true
             (clj-eval/eval-expr model (make-expr :lit-bool true) env)))
      (is (= nil
             (clj-eval/eval-expr model (make-expr :lit-nil) env))))))

(deftest test-primitives
  (testing "Primitive operations"
    (let [model (clj-eval/->StandardEval)
          env {}
          add-prim (clj-eval/eval-expr model (make-expr :prim-add) env)
          sub-prim (clj-eval/eval-expr model (make-expr :prim-sub) env)]
      (is (= :primitive (:type add-prim)))
      ;; Primitives are Clojure functions wrapped
      (is (= 7 ((:fn add-prim) 7)))
      (is (= -3 ((:fn sub-prim) 3))))))

(deftest test-identity-function
  (testing "Identity function: (fn [x] x)"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [x] x) - note params is now a list
          id-fn (make-expr :fn-expr ['x] (make-expr :var 'x))
          id-val (clj-eval/eval-expr model id-fn env)]
      (is (= :closure (:type id-val)))
      (is (= ['x] (:params id-val)))  ; params not param

      ;; Apply to value: ((fn [x] x) 42) - args is now a list
      (let [app-expr (make-expr :app id-fn [(make-expr :lit-num 42)])
            result (clj-eval/eval-expr model app-expr env)]
        (is (= 42 result))))))

(deftest test-constant-function
  (testing "Constant function: (fn [x] 5)"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [x] 5)
          const-fn (make-expr :fn-expr ['x] (make-expr :lit-num 5))
          ;; ((fn [x] 5) 42)
          app-expr (make-expr :app const-fn [(make-expr :lit-num 42)])
          result (clj-eval/eval-expr model app-expr env)]
      (is (= 5 result)))))

(deftest test-addition
  (testing "Addition using primitive works"
    (let [model (clj-eval/->StandardEval)
          env {'+ (clj-eval/make-primitive +)}
          ;; Binary addition: ((fn [x y] (+ x y)) 2 3)
          ;; Simplified: just verify primitives can be stored in env
          result (clj-eval/eval-expr model (make-expr :var '+) env)]
      ;; Can look up primitives from environment
      (is (= :primitive (:type result)))
      (is (= 5 ((:fn result) 2 3))))))

(deftest test-let-binding
  (testing "Let binding: (let [x 42] x)"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (let [x 42] x)
          let-expr (make-expr :let-expr
                              'x
                              (make-expr :lit-num 42)
                              (make-expr :var 'x))
          result (clj-eval/eval-expr model let-expr env)]
      (is (= 42 result)))))

(deftest test-let-shadowing
  (testing "Let shadowing: (let [x 5] (let [x 10] x))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (let [x 10] x)
          inner-let (make-expr :let-expr
                               'x
                               (make-expr :lit-num 10)
                               (make-expr :var 'x))
          ;; (let [x 5] (let [x 10] x))
          outer-let (make-expr :let-expr
                               'x
                               (make-expr :lit-num 5)
                               inner-let)
          result (clj-eval/eval-expr model outer-let env)]
      (is (= 10 result)))))

(deftest test-if-true
  (testing "If with true test: (if true 1 2)"
    (let [model (clj-eval/->StandardEval)
          env {}
          if-expr (make-expr :if-expr
                             (make-expr :lit-bool true)
                             (make-expr :lit-num 1)
                             (make-expr :lit-num 2))
          result (clj-eval/eval-expr model if-expr env)]
      (is (= 1 result)))))

(deftest test-if-false
  (testing "If with false test: (if false 1 2)"
    (let [model (clj-eval/->StandardEval)
          env {}
          if-expr (make-expr :if-expr
                             (make-expr :lit-bool false)
                             (make-expr :lit-num 1)
                             (make-expr :lit-num 2))
          result (clj-eval/eval-expr model if-expr env)]
      (is (= 2 result)))))

(deftest test-if-nil-is-falsy
  (testing "If with nil test (should be falsy): (if nil 1 2)"
    (let [model (clj-eval/->StandardEval)
          env {}
          if-expr (make-expr :if-expr
                             (make-expr :lit-nil)
                             (make-expr :lit-num 1)
                             (make-expr :lit-num 2))
          result (clj-eval/eval-expr model if-expr env)]
      (is (= 2 result)))))

(deftest test-do-sequencing
  (testing "Do sequencing: (do 1 2) => 2"
    (let [model (clj-eval/->StandardEval)
          env {}
          do-expr (make-expr :do-expr
                             (make-expr :lit-num 1)
                             (make-expr :lit-num 2))
          result (clj-eval/eval-expr model do-expr env)]
      (is (= 2 result)))))

(deftest test-closure-captures-environment
  (testing "Closure captures environment: (let [y 10] (fn [x] y))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [x] y) - references y from outer scope
          fn-expr (make-expr :fn-expr ['x] (make-expr :var 'y))
          ;; (let [y 10] (fn [x] y))
          let-expr (make-expr :let-expr
                              'y
                              (make-expr :lit-num 10)
                              fn-expr)
          closure (clj-eval/eval-expr model let-expr env)]
      (is (= :closure (:type closure)))
      (is (= {'y 10} (:env closure)))

      ;; Apply closure: should return 10 regardless of argument
      (let [app-expr (make-expr :app
                                let-expr
                                [(make-expr :lit-num 999)])
            result (clj-eval/eval-expr model app-expr env)]
        (is (= 10 result))))))

(deftest test-nested-functions
  (testing "Nested functions: (fn [x] (fn [y] x))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [y] x)
          inner-fn (make-expr :fn-expr ['y] (make-expr :var 'x))
          ;; (fn [x] (fn [y] x))
          outer-fn (make-expr :fn-expr ['x] inner-fn)
          ;; ((fn [x] (fn [y] x)) 42)
          app-outer (make-expr :app outer-fn [(make-expr :lit-num 42)])
          inner-closure (clj-eval/eval-expr model app-outer env)]
      (is (= :closure (:type inner-closure)))
      (is (= {'x 42} (:env inner-closure)))

      ;; (((fn [x] (fn [y] x)) 42) 99) => 42
      (let [app-inner (make-expr :app app-outer [(make-expr :lit-num 99)])
            result (clj-eval/eval-expr model app-inner env)]
        (is (= 42 result))))))

(deftest test-symbolic-model
  (testing "Symbolic model builds ASTs without evaluation"
    (let [model (clj-sym/->SymbolicClojure)]
      ;; Symbolic models just build AST nodes without evaluation
      ;; We can verify the model was created
      (is (not (nil? model)))
      ;; TODO: Add symbolic expression building tests once we understand
      ;; the symbolic model API better
      )))

(deftest test-complex-example
  (testing "Complex example with let and primitives"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (let [x 5] (let [y 10] (if true x y)))
          inner-let (make-expr :let-expr
                               'y
                               (make-expr :lit-num 10)
                               (make-expr :if-expr
                                          (make-expr :lit-bool true)
                                          (make-expr :var 'x)
                                          (make-expr :var 'y)))
          outer-let (make-expr :let-expr
                               'x
                               (make-expr :lit-num 5)
                               inner-let)
          result (clj-eval/eval-expr model outer-let env)]
      ;; Should return x=5, not y=10
      (is (= 5 result)))))

(deftest test-unbound-variable-error
  (testing "Unbound variable throws error"
    (let [model (clj-eval/->StandardEval)
          env {}
          var-expr (make-expr :var 'undefined)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Unbound variable"
           (clj-eval/eval-expr model var-expr env))))))

(deftest test-theory-introspection
  (testing "Can introspect the theory"
    (is (gat-core/gat? clj-core/ThClojureCore))
    (is (some? (:type-constructors clj-core/ThClojureCore)))
    (is (some? (:term-constructors clj-core/ThClojureCore)))

    ;; Check that operations exist
    (let [term-names (set (map #(-> % :term :head :name) (:term-constructors clj-core/ThClojureCore)))]
      (is (contains? term-names 'lit-num))
      (is (contains? term-names 'var))
      (is (contains? term-names 'fn-expr))
      (is (contains? term-names 'app))
      (is (contains? term-names 'let-expr))
      (is (contains? term-names 'if-expr)))))

;; ============================================================================
;; Multi-arity function tests
;; ============================================================================

(deftest test-binary-function
  (testing "Binary function: (fn [x y] (+ x y))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [x y] (+ x y))
          add-fn (make-expr :fn-expr
                            ['x 'y]
                            (make-expr :app
                                       (make-expr :prim-add)
                                       [(make-expr :var 'x)
                                        (make-expr :var 'y)]))
          ;; ((fn [x y] (+ x y)) 2 3)
          app-expr (make-expr :app
                              add-fn
                              [(make-expr :lit-num 2)
                               (make-expr :lit-num 3)])
          result (clj-eval/eval-expr model app-expr env)]
      (is (= 5 result)))))

(deftest test-ternary-function
  (testing "Ternary function: (fn [x y z] (+ (+ x y) z))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [x y z] (+ (+ x y) z))
          add3-fn (make-expr :fn-expr
                             ['x 'y 'z]
                             (make-expr :app
                                        (make-expr :prim-add)
                                        [(make-expr :app
                                                    (make-expr :prim-add)
                                                    [(make-expr :var 'x)
                                                     (make-expr :var 'y)])
                                         (make-expr :var 'z)]))
          ;; ((fn [x y z] ...) 1 2 3)
          app-expr (make-expr :app
                              add3-fn
                              [(make-expr :lit-num 1)
                               (make-expr :lit-num 2)
                               (make-expr :lit-num 3)])
          result (clj-eval/eval-expr model app-expr env)]
      (is (= 6 result)))))

(deftest test-arity-mismatch-error
  (testing "Arity mismatch throws error"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (fn [x y] (+ x y))
          binary-fn (make-expr :fn-expr
                               ['x 'y]
                               (make-expr :app
                                          (make-expr :prim-add)
                                          [(make-expr :var 'x)
                                           (make-expr :var 'y)]))
          ;; Try to call with 1 argument (should fail)
          app-expr (make-expr :app
                              binary-fn
                              [(make-expr :lit-num 42)])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Arity mismatch"
           (clj-eval/eval-expr model app-expr env))))))

;; ============================================================================
;; Recursion tests
;; ============================================================================

(deftest test-factorial
  (testing "Factorial using letrec: (letrec [fact (fn [n] ...)] (fact 5))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (letrec [fact (fn [n] (if (= n 0) 1 (* n (fact (- n 1)))))]
          ;;   (fact 5))
          letrec-expr (make-expr :letrec-expr
                                 'fact
                                 ['n]
                                ;; if (= n 0) 1 (* n (fact (- n 1)))
                                 (make-expr :if-expr
                                          ;; test: (= n 0)
                                            (make-expr :app
                                                       (make-expr :prim-eq)
                                                       [(make-expr :var 'n)
                                                        (make-expr :lit-num 0)])
                                          ;; then: 1
                                            (make-expr :lit-num 1)
                                          ;; else: (* n (fact (- n 1)))
                                            (make-expr :app
                                                       (make-expr :prim-mul)
                                                       [(make-expr :var 'n)
                                                        (make-expr :app
                                                                   (make-expr :var 'fact)
                                                                   [(make-expr :app
                                                                               (make-expr :prim-sub)
                                                                               [(make-expr :var 'n)
                                                                                (make-expr :lit-num 1)])])]))
                                ;; in-expr: (fact 5)
                                 (make-expr :app
                                            (make-expr :var 'fact)
                                            [(make-expr :lit-num 5)]))
          result (clj-eval/eval-expr model letrec-expr env)]
      (is (= 120 result)))))

(deftest test-fibonacci
  (testing "Fibonacci using letrec: (letrec [fib (fn [n] ...)] (fib 6))"
    (let [model (clj-eval/->StandardEval)
          env {}
          ;; (letrec [fib (fn [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))]
          ;;   (fib 6))
          letrec-expr (make-expr :letrec-expr
                                 'fib
                                 ['n]
                                ;; if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))
                                 (make-expr :if-expr
                                          ;; test: (< n 2)
                                            (make-expr :app
                                                       (make-expr :prim-lt)
                                                       [(make-expr :var 'n)
                                                        (make-expr :lit-num 2)])
                                          ;; then: n
                                            (make-expr :var 'n)
                                          ;; else: (+ (fib (- n 1)) (fib (- n 2)))
                                            (make-expr :app
                                                       (make-expr :prim-add)
                                                       [(make-expr :app
                                                                   (make-expr :var 'fib)
                                                                   [(make-expr :app
                                                                               (make-expr :prim-sub)
                                                                               [(make-expr :var 'n)
                                                                                (make-expr :lit-num 1)])])
                                                        (make-expr :app
                                                                   (make-expr :var 'fib)
                                                                   [(make-expr :app
                                                                               (make-expr :prim-sub)
                                                                               [(make-expr :var 'n)
                                                                                (make-expr :lit-num 2)])])]))
                                ;; in-expr: (fib 6)
                                 (make-expr :app
                                            (make-expr :var 'fib)
                                            [(make-expr :lit-num 6)]))
          result (clj-eval/eval-expr model letrec-expr env)]
      (is (= 8 result)))))

;; ============================================================================
;; Data structure tests
;; ============================================================================

(deftest test-vector-literal
  (testing "Vector literal: [1 2 3]"
    (let [model (clj-eval/->StandardEval)
          env {}
          vec-expr (make-expr :lit-vec
                              [(make-expr :lit-num 1)
                               (make-expr :lit-num 2)
                               (make-expr :lit-num 3)])
          result (clj-eval/eval-expr model vec-expr env)]
      (is (= [1 2 3] result)))))

(deftest test-vector-nth
  (testing "Vector indexing: (nth [1 2 3] 1) => 2"
    (let [model (clj-eval/->StandardEval)
          env {}
          vec-expr (make-expr :lit-vec
                              [(make-expr :lit-num 1)
                               (make-expr :lit-num 2)
                               (make-expr :lit-num 3)])
          ;; (nth vec 1)
          nth-expr (make-expr :app
                              (make-expr :prim-nth)
                              [vec-expr
                               (make-expr :lit-num 1)])
          result (clj-eval/eval-expr model nth-expr env)]
      (is (= 2 result)))))

(deftest test-vector-count
  (testing "Vector count: (count [1 2 3]) => 3"
    (let [model (clj-eval/->StandardEval)
          env {}
          vec-expr (make-expr :lit-vec
                              [(make-expr :lit-num 1)
                               (make-expr :lit-num 2)
                               (make-expr :lit-num 3)])
          ;; (count vec)
          count-expr (make-expr :app
                                (make-expr :prim-count)
                                [vec-expr])
          result (clj-eval/eval-expr model count-expr env)]
      (is (= 3 result)))))

(deftest test-vector-conj
  (testing "Vector conj: (conj [1 2] 3) => [1 2 3]"
    (let [model (clj-eval/->StandardEval)
          env {}
          vec-expr (make-expr :lit-vec
                              [(make-expr :lit-num 1)
                               (make-expr :lit-num 2)])
          ;; (conj vec 3)
          conj-expr (make-expr :app
                               (make-expr :prim-conj)
                               [vec-expr
                                (make-expr :lit-num 3)])
          result (clj-eval/eval-expr model conj-expr env)]
      (is (= [1 2 3] result)))))

(deftest test-map-literal
  (testing "Map literal: {:x 10, :y 20}"
    (let [model (clj-eval/->StandardEval)
          env {}
          map-expr (make-expr :lit-map
                              [[(make-expr :lit-str "x")
                                (make-expr :lit-num 10)]
                               [(make-expr :lit-str "y")
                                (make-expr :lit-num 20)]])
          result (clj-eval/eval-expr model map-expr env)]
      (is (= {"x" 10 "y" 20} result)))))

(deftest test-map-get
  (testing "Map get: (get {:x 10} :x) => 10"
    (let [model (clj-eval/->StandardEval)
          env {}
          map-expr (make-expr :lit-map
                              [[(make-expr :lit-str "x")
                                (make-expr :lit-num 10)]
                               [(make-expr :lit-str "y")
                                (make-expr :lit-num 20)]])
          ;; (get map "x")
          get-expr (make-expr :app
                              (make-expr :prim-get)
                              [map-expr
                               (make-expr :lit-str "x")])
          result (clj-eval/eval-expr model get-expr env)]
      (is (= 10 result)))))

(deftest test-map-assoc
  (testing "Map assoc: (assoc {:x 10} :z 30) => {:x 10, :z 30}"
    (let [model (clj-eval/->StandardEval)
          env {}
          map-expr (make-expr :lit-map
                              [[(make-expr :lit-str "x")
                                (make-expr :lit-num 10)]])
          ;; (assoc map "z" 30)
          assoc-expr (make-expr :app
                                (make-expr :prim-assoc)
                                [map-expr
                                 (make-expr :lit-str "z")
                                 (make-expr :lit-num 30)])
          result (clj-eval/eval-expr model assoc-expr env)]
      (is (= {"x" 10 "z" 30} result)))))
