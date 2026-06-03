(ns katzen.examples.clojure-symbolic-test
  "Tests for symbolic model operations: pretty-printing, free variable analysis,
  and constant folding optimization."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.examples.clojure-symbolic :as sym]))

;; ============================================================================
;; Pretty Printing Tests
;; ============================================================================

(deftest test-pretty-print-literals
  (testing "Pretty-print literal values"
    (is (= 42 (sym/pretty-print (sym/make-expr :lit-num 42))))
    (is (= "hello" (sym/pretty-print (sym/make-expr :lit-str "hello"))))
    (is (= true (sym/pretty-print (sym/make-expr :lit-bool true))))
    (is (= nil (sym/pretty-print (sym/make-expr :lit-nil))))))

(deftest test-pretty-print-variables
  (testing "Pretty-print variables"
    (is (= 'x (sym/pretty-print (sym/make-expr :var 'x))))))

(deftest test-pretty-print-functions
  (testing "Pretty-print function expressions"
    (is (= '(fn [x] x)
           (sym/pretty-print
            (sym/make-expr :fn-expr ['x] (sym/make-expr :var 'x)))))

    (is (= '(fn [x y] (+ x y))
           (sym/pretty-print
            (sym/make-expr :fn-expr ['x 'y]
                           (sym/make-expr :app (sym/make-expr :prim-add)
                                          [(sym/make-expr :var 'x)
                                           (sym/make-expr :var 'y)])))))))

(deftest test-pretty-print-application
  (testing "Pretty-print function application"
    (is (= '(f x)
           (sym/pretty-print
            (sym/make-expr :app (sym/make-expr :var 'f)
                           [(sym/make-expr :var 'x)]))))

    (is (= '(+ 2 3)
           (sym/pretty-print
            (sym/make-expr :app (sym/make-expr :prim-add)
                           [(sym/make-expr :lit-num 2)
                            (sym/make-expr :lit-num 3)]))))))

(deftest test-pretty-print-let
  (testing "Pretty-print let expressions"
    (is (= '(let [x 5] x)
           (sym/pretty-print
            (sym/make-expr :let-expr 'x
                           (sym/make-expr :lit-num 5)
                           (sym/make-expr :var 'x)))))))

(deftest test-pretty-print-letrec
  (testing "Pretty-print letrec expressions"
    (is (= '(letrec [fact (fn [n] n)] (fact 5))
           (sym/pretty-print
            (sym/make-expr :letrec-expr 'fact ['n]
                           (sym/make-expr :var 'n)
                           (sym/make-expr :app (sym/make-expr :var 'fact)
                                          [(sym/make-expr :lit-num 5)])))))))

(deftest test-pretty-print-if
  (testing "Pretty-print if expressions"
    (is (= '(if true 1 2)
           (sym/pretty-print
            (sym/make-expr :if-expr
                           (sym/make-expr :lit-bool true)
                           (sym/make-expr :lit-num 1)
                           (sym/make-expr :lit-num 2)))))))

(deftest test-pretty-print-data-structures
  (testing "Pretty-print vectors"
    (is (= [1 2 3]
           (sym/pretty-print
            (sym/make-expr :lit-vec
                           [(sym/make-expr :lit-num 1)
                            (sym/make-expr :lit-num 2)
                            (sym/make-expr :lit-num 3)])))))

  (testing "Pretty-print maps"
    (is (= {"x" 10, "y" 20}
           (sym/pretty-print
            (sym/make-expr :lit-map
                           [[(sym/make-expr :lit-str "x")
                             (sym/make-expr :lit-num 10)]
                            [(sym/make-expr :lit-str "y")
                             (sym/make-expr :lit-num 20)]]))))))

;; ============================================================================
;; Free Variable Analysis Tests
;; ============================================================================

(deftest test-free-vars-literals
  (testing "Literals have no free variables"
    (is (= #{} (sym/free-vars (sym/make-expr :lit-num 42))))
    (is (= #{} (sym/free-vars (sym/make-expr :lit-str "hello"))))
    (is (= #{} (sym/free-vars (sym/make-expr :lit-bool true))))))

(deftest test-free-vars-variable
  (testing "Variable is free"
    (is (= #{'x} (sym/free-vars (sym/make-expr :var 'x))))))

(deftest test-free-vars-function
  (testing "Function parameters bind variables"
    ;; (fn [x] x) - x is bound, no free vars
    (is (= #{}
           (sym/free-vars
            (sym/make-expr :fn-expr ['x] (sym/make-expr :var 'x)))))

    ;; (fn [x] y) - y is free
    (is (= #{'y}
           (sym/free-vars
            (sym/make-expr :fn-expr ['x] (sym/make-expr :var 'y)))))

    ;; (fn [x] (+ x y)) - y is free
    (is (= #{'y}
           (sym/free-vars
            (sym/make-expr :fn-expr ['x]
                           (sym/make-expr :app (sym/make-expr :prim-add)
                                          [(sym/make-expr :var 'x)
                                           (sym/make-expr :var 'y)])))))))

(deftest test-free-vars-application
  (testing "Application collects free vars from function and args"
    ;; (f x) - both f and x are free
    (is (= #{'f 'x}
           (sym/free-vars
            (sym/make-expr :app (sym/make-expr :var 'f)
                           [(sym/make-expr :var 'x)]))))

    ;; ((fn [y] y) x) - only x is free
    (is (= #{'x}
           (sym/free-vars
            (sym/make-expr :app
                           (sym/make-expr :fn-expr ['y] (sym/make-expr :var 'y))
                           [(sym/make-expr :var 'x)]))))))

(deftest test-free-vars-let
  (testing "Let binds variable in body"
    ;; (let [x 5] x) - no free vars
    (is (= #{}
           (sym/free-vars
            (sym/make-expr :let-expr 'x
                           (sym/make-expr :lit-num 5)
                           (sym/make-expr :var 'x)))))

    ;; (let [x y] x) - y is free (from val-expr)
    (is (= #{'y}
           (sym/free-vars
            (sym/make-expr :let-expr 'x
                           (sym/make-expr :var 'y)
                           (sym/make-expr :var 'x)))))

    ;; (let [x 5] y) - y is free
    (is (= #{'y}
           (sym/free-vars
            (sym/make-expr :let-expr 'x
                           (sym/make-expr :lit-num 5)
                           (sym/make-expr :var 'y)))))))

(deftest test-free-vars-letrec
  (testing "Letrec binds name in both function and in-expr"
    ;; (letrec [fact (fn [n] (fact n))] (fact 5))
    ;; No free vars - fact is bound recursively
    (is (= #{}
           (sym/free-vars
            (sym/make-expr :letrec-expr 'fact ['n]
                           (sym/make-expr :app (sym/make-expr :var 'fact)
                                          [(sym/make-expr :var 'n)])
                           (sym/make-expr :app (sym/make-expr :var 'fact)
                                          [(sym/make-expr :lit-num 5)])))))))

(deftest test-free-vars-nested
  (testing "Nested closures capture outer variables"
    ;; (fn [x] (fn [y] x)) - no free vars
    (is (= #{}
           (sym/free-vars
            (sym/make-expr :fn-expr ['x]
                           (sym/make-expr :fn-expr ['y]
                                          (sym/make-expr :var 'x))))))

    ;; (fn [x] (fn [y] z)) - z is free
    (is (= #{'z}
           (sym/free-vars
            (sym/make-expr :fn-expr ['x]
                           (sym/make-expr :fn-expr ['y]
                                          (sym/make-expr :var 'z))))))))

(deftest test-closed-predicate
  (testing "closed? predicate"
    (is (sym/closed? (sym/make-expr :lit-num 42)))
    (is (sym/closed? (sym/make-expr :fn-expr ['x] (sym/make-expr :var 'x))))
    (is (not (sym/closed? (sym/make-expr :var 'x))))
    (is (not (sym/closed? (sym/make-expr :fn-expr ['x] (sym/make-expr :var 'y)))))))

(deftest test-bound-vars
  (testing "bound-vars collects all binding names"
    ;; (fn [x] x) - x is bound
    (is (= #{'x}
           (sym/bound-vars
            (sym/make-expr :fn-expr ['x] (sym/make-expr :var 'x)))))

    ;; (let [x 5] (let [y 10] x)) - both x and y are bound
    (is (= #{'x 'y}
           (sym/bound-vars
            (sym/make-expr :let-expr 'x
                           (sym/make-expr :lit-num 5)
                           (sym/make-expr :let-expr 'y
                                          (sym/make-expr :lit-num 10)
                                          (sym/make-expr :var 'x))))))))

;; ============================================================================
;; Constant Folding Tests
;; ============================================================================

(deftest test-constant-fold-arithmetic
  (testing "Fold arithmetic operations on constants"
    ;; (+ 2 3) => 5
    (is (= (sym/make-expr :lit-num 5)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-add)
                           [(sym/make-expr :lit-num 2)
                            (sym/make-expr :lit-num 3)]))))

    ;; (* 4 5) => 20
    (is (= (sym/make-expr :lit-num 20)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-mul)
                           [(sym/make-expr :lit-num 4)
                            (sym/make-expr :lit-num 5)]))))

    ;; (- 10 3) => 7
    (is (= (sym/make-expr :lit-num 7)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-sub)
                           [(sym/make-expr :lit-num 10)
                            (sym/make-expr :lit-num 3)]))))))

(deftest test-constant-fold-comparison
  (testing "Fold comparison operations on constants"
    ;; (= 5 5) => true
    (is (= (sym/make-expr :lit-bool true)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-eq)
                           [(sym/make-expr :lit-num 5)
                            (sym/make-expr :lit-num 5)]))))

    ;; (< 3 10) => true
    (is (= (sym/make-expr :lit-bool true)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-lt)
                           [(sym/make-expr :lit-num 3)
                            (sym/make-expr :lit-num 10)]))))

    ;; (> 3 10) => false
    (is (= (sym/make-expr :lit-bool false)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-gt)
                           [(sym/make-expr :lit-num 3)
                            (sym/make-expr :lit-num 10)]))))))

(deftest test-constant-fold-if-true
  (testing "Fold if with constant true test"
    ;; (if true 1 2) => 1
    (is (= (sym/make-expr :lit-num 1)
           (sym/constant-fold
            (sym/make-expr :if-expr
                           (sym/make-expr :lit-bool true)
                           (sym/make-expr :lit-num 1)
                           (sym/make-expr :lit-num 2)))))))

(deftest test-constant-fold-if-false
  (testing "Fold if with constant false test"
    ;; (if false 1 2) => 2
    (is (= (sym/make-expr :lit-num 2)
           (sym/constant-fold
            (sym/make-expr :if-expr
                           (sym/make-expr :lit-bool false)
                           (sym/make-expr :lit-num 1)
                           (sym/make-expr :lit-num 2)))))))

(deftest test-constant-fold-nested
  (testing "Fold nested constant expressions"
    ;; (+ (* 2 3) 4) => (+ 6 4) => 10
    (is (= (sym/make-expr :lit-num 10)
           (sym/constant-fold
            (sym/make-expr :app (sym/make-expr :prim-add)
                           [(sym/make-expr :app (sym/make-expr :prim-mul)
                                           [(sym/make-expr :lit-num 2)
                                            (sym/make-expr :lit-num 3)])
                            (sym/make-expr :lit-num 4)]))))))

(deftest test-constant-fold-dead-let
  (testing "Eliminate dead let binding"
    ;; (let [x 5] 10) => 10 (x is unused)
    (is (= (sym/make-expr :lit-num 10)
           (sym/constant-fold
            (sym/make-expr :let-expr 'x
                           (sym/make-expr :lit-num 5)
                           (sym/make-expr :lit-num 10)))))))

(deftest test-constant-fold-preserves-non-constants
  (testing "Don't fold expressions with variables"
    ;; (+ x 3) should not fold (x is not constant)
    (let [expr (sym/make-expr :app (sym/make-expr :prim-add)
                              [(sym/make-expr :var 'x)
                               (sym/make-expr :lit-num 3)])]
      (is (= expr (sym/constant-fold expr))))))

(deftest test-constant-fold-in-function-body
  (testing "Fold constants inside function bodies"
    ;; (fn [x] (+ 2 3)) => (fn [x] 5)
    (is (= (sym/make-expr :fn-expr ['x] (sym/make-expr :lit-num 5))
           (sym/constant-fold
            (sym/make-expr :fn-expr ['x]
                           (sym/make-expr :app (sym/make-expr :prim-add)
                                          [(sym/make-expr :lit-num 2)
                                           (sym/make-expr :lit-num 3)])))))))

(deftest test-optimize-fixpoint
  (testing "Optimize until fixpoint"
    ;; (if (= 5 5) (+ 2 3) 10) => (if true 5 10) => 5
    (is (= (sym/make-expr :lit-num 5)
           (sym/optimize-until-fixpoint
            (sym/make-expr :if-expr
                           (sym/make-expr :app (sym/make-expr :prim-eq)
                                          [(sym/make-expr :lit-num 5)
                                           (sym/make-expr :lit-num 5)])
                           (sym/make-expr :app (sym/make-expr :prim-add)
                                          [(sym/make-expr :lit-num 2)
                                           (sym/make-expr :lit-num 3)])
                           (sym/make-expr :lit-num 10)))))))

;; ============================================================================
;; Integration Tests
;; ============================================================================

(deftest test-pretty-print-optimized
  (testing "Pretty-print optimized expressions"
    (let [original (sym/make-expr :app (sym/make-expr :prim-add)
                                  [(sym/make-expr :lit-num 2)
                                   (sym/make-expr :lit-num 3)])
          optimized (sym/constant-fold original)]
      (is (= 5 (sym/pretty-print optimized))))))

(deftest test-free-vars-after-optimization
  (testing "Free vars are preserved after optimization"
    ;; (+ x 3) should still have x as free var after optimization
    (let [expr (sym/make-expr :app (sym/make-expr :prim-add)
                              [(sym/make-expr :var 'x)
                               (sym/make-expr :lit-num 3)])
          optimized (sym/constant-fold expr)]
      (is (= #{'x} (sym/free-vars optimized))))))
