(ns katzen.library.algebra-test
  "Tests for algebraic theory library."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.library.algebra :as alg]
            [katzen.library.algebra.th-semigroup :as semi]
            [katzen.library.algebra.th-commutative-monoid :as cmon]
            [katzen.library.algebra.th-group :as grp]
            [katzen.library.algebra.th-abelian-group :as abel]
            [katzen.library.algebra-examples :as ex]
            [katzen.model :as model]))

;;; ============================================================================
;;; Theory Introspection Tests
;;; ============================================================================

(deftest test-semigroup-theory
  (testing "ThSemigroup has correct structure"
    (is (some? alg/ThSemigroup))
    (is (= 'ThSemigroup (:name alg/ThSemigroup)))
    (is (= 1 (count (:type-constructors alg/ThSemigroup))))
    (is (= 1 (count (:term-constructors alg/ThSemigroup))))
    (is (= 1 (count (:axioms alg/ThSemigroup))))))

(deftest test-commutative-monoid-theory
  (testing "ThCommutativeMonoid has correct structure"
    (is (some? alg/ThCommutativeMonoid))
    (is (= 'ThCommutativeMonoid (:name alg/ThCommutativeMonoid)))
    (is (= 1 (count (:type-constructors alg/ThCommutativeMonoid))))
    (is (= 2 (count (:term-constructors alg/ThCommutativeMonoid))))
    (is (= 4 (count (:axioms alg/ThCommutativeMonoid))))))

(deftest test-group-theory
  (testing "ThGroup has correct structure"
    (is (some? alg/ThGroup))
    (is (= 'ThGroup (:name alg/ThGroup)))
    (is (= 1 (count (:type-constructors alg/ThGroup))))
    (is (= 3 (count (:term-constructors alg/ThGroup))))  ; mul, unit, inv
    (is (= 5 (count (:axioms alg/ThGroup))))))  ; assoc, left-unit, right-unit, left-inv, right-inv

(deftest test-abelian-group-theory
  (testing "ThAbelianGroup has correct structure"
    (is (some? alg/ThAbelianGroup))
    (is (= 'ThAbelianGroup (:name alg/ThAbelianGroup)))
    (is (= 1 (count (:type-constructors alg/ThAbelianGroup))))
    (is (= 3 (count (:term-constructors alg/ThAbelianGroup))))
    (is (= 6 (count (:axioms alg/ThAbelianGroup))))))  ; + commutativity

;;; ============================================================================
;;; Semigroup Model Tests
;;; ============================================================================

(deftest test-nat-add-semigroup
  (testing "Natural numbers under addition form a semigroup"
    (let [m (ex/->NatAddSemigroup)]
      (is (model/concrete-model? m))
      (is (= alg/ThSemigroup (model/theory m)))

      ;; Operation works
      (is (= 5 (semi/mul m 2 3)))
      (is (= 10 (semi/mul m 4 6)))

      ;; Associativity
      (is (= (semi/mul m (semi/mul m 1 2) 3)
             (semi/mul m 1 (semi/mul m 2 3)))))))

(deftest test-string-semigroup
  (testing "Strings under concatenation form a semigroup"
    (let [m (ex/->StringSemigroup)]
      (is (= "helloworld" (semi/mul m "hello" "world")))
      (is (= "abc" (semi/mul m "a" "bc")))

      ;; Associativity
      (is (= (semi/mul m (semi/mul m "a" "b") "c")
             (semi/mul m "a" (semi/mul m "b" "c")))))))

(deftest test-symbolic-semigroup
  (testing "Symbolic semigroup builds expressions"
    (let [m (ex/->SymSemigroup)]
      (is (model/symbolic-model? m))
      (let [expr (semi/mul m 'x 'y)]
        (is (model/symbolic-expr? expr))
        (is (= 'mul (:head expr)))
        (is (= ['x 'y] (:args expr)))))))

;;; ============================================================================
;;; Commutative Monoid Model Tests
;;; ============================================================================

(deftest test-nat-add-monoid
  (testing "Natural numbers under addition form a commutative monoid"
    (let [m (ex/->NatAddMonoid)]
      (is (model/concrete-model? m))
      (is (= alg/ThCommutativeMonoid (model/theory m)))

      ;; Unit exists
      (is (= 0 (cmon/unit m)))

      ;; Unit laws
      (is (= 5 (cmon/mul m (cmon/unit m) 5)))
      (is (= 5 (cmon/mul m 5 (cmon/unit m))))

      ;; Associativity
      (is (= (cmon/mul m (cmon/mul m 2 3) 4)
             (cmon/mul m 2 (cmon/mul m 3 4))))

      ;; Commutativity
      (is (= (cmon/mul m 3 5) (cmon/mul m 5 3))))))

(deftest test-bool-and-monoid
  (testing "Booleans under AND form a commutative monoid"
    (let [m (ex/->BoolAndMonoid)]
      (is (= true (cmon/unit m)))
      (is (= false (cmon/mul m true false)))
      (is (= true (cmon/mul m true true)))

      ;; Unit laws
      (is (= true (cmon/mul m (cmon/unit m) true)))
      (is (= false (cmon/mul m (cmon/unit m) false)))

      ;; Commutativity
      (is (= (cmon/mul m true false) (cmon/mul m false true))))))

(deftest test-bool-or-monoid
  (testing "Booleans under OR form a commutative monoid"
    (let [m (ex/->BoolOrMonoid)]
      (is (= false (cmon/unit m)))
      (is (= true (cmon/mul m true false)))
      (is (= false (cmon/mul m false false)))

      ;; Unit laws
      (is (= true (cmon/mul m (cmon/unit m) true)))
      (is (= false (cmon/mul m (cmon/unit m) false)))

      ;; Commutativity
      (is (= (cmon/mul m true false) (cmon/mul m false true))))))

;;; ============================================================================
;;; Group Model Tests
;;; ============================================================================

(deftest test-int-add-group
  (testing "Integers under addition form a group"
    (let [m (ex/->IntAddGroup)]
      (is (model/concrete-model? m))
      (is (= alg/ThGroup (model/theory m)))

      ;; Unit
      (is (= 0 (grp/unit m)))

      ;; Inverse
      (is (= -5 (grp/inv m 5)))
      (is (= 5 (grp/inv m -5)))
      (is (= 0 (grp/inv m 0)))

      ;; Unit laws
      (is (= 7 (grp/mul m (grp/unit m) 7)))
      (is (= 7 (grp/mul m 7 (grp/unit m))))

      ;; Inverse laws
      (is (= (grp/unit m) (grp/mul m (grp/inv m 5) 5)))
      (is (= (grp/unit m) (grp/mul m 5 (grp/inv m 5))))

      ;; Associativity
      (is (= (grp/mul m (grp/mul m 2 3) 4)
             (grp/mul m 2 (grp/mul m 3 4)))))))

(deftest test-int-mul-units-group
  (testing "Multiplicative group of units {-1, 1}"
    (let [m (ex/->IntMulUnitsGroup)]
      (is (= 1 (grp/unit m)))
      (is (= -1 (grp/mul m -1 1)))
      (is (= 1 (grp/mul m -1 -1)))

      ;; Inverse (self-inverse)
      (is (= 1 (grp/inv m 1)))
      (is (= -1 (grp/inv m -1)))

      ;; Inverse laws
      (is (= (grp/unit m) (grp/mul m (grp/inv m -1) -1))))))

(deftest test-modulo-add-group
  (testing "Integers modulo n under addition form a group"
    (let [m (ex/->ModuloAddGroup)]
      ;; Unit
      (is (= 0 (grp/unit m)))

      ;; Addition modulo 5
      (is (= 2 (grp/mul m 3 4)))  ; 3 + 4 = 7 ≡ 2 (mod 5)
      (is (= 1 (grp/mul m 2 4)))  ; 2 + 4 = 6 ≡ 1 (mod 5)

      ;; Inverse
      (is (= 2 (grp/inv m 3)))    ; 3 + 2 = 5 ≡ 0 (mod 5)
      (is (= 4 (grp/inv m 1)))    ; 1 + 4 = 5 ≡ 0 (mod 5)

      ;; Inverse laws
      (is (= (grp/unit m) (grp/mul m 3 (grp/inv m 3)))))))

;;; ============================================================================
;;; Abelian Group Model Tests
;;; ============================================================================

(deftest test-int-add-abelian-group
  (testing "Integers under addition form an abelian group"
    (let [m (ex/->IntAddAbelianGroup)]
      (is (model/concrete-model? m))
      (is (= alg/ThAbelianGroup (model/theory m)))

      ;; All group laws (inherited)
      (is (= 0 (abel/unit m)))
      (is (= -5 (abel/inv m 5)))

      ;; Commutativity
      (is (= (abel/mul m 3 7) (abel/mul m 7 3)))
      (is (= (abel/mul m -2 5) (abel/mul m 5 -2))))))

(deftest test-vector-add-abelian-group
  (testing "Vectors under addition form an abelian group"
    (let [m (ex/->VectorAddAbelianGroup)]
      ;; Unit (empty vector)
      (is (= [] (abel/unit m)))

      ;; Addition
      (is (= [5 7 9] (abel/mul m [1 2 3] [4 5 6])))
      (is (= [0 0 0] (abel/mul m [1 2 3] [-1 -2 -3])))

      ;; Inverse
      (is (= [-1 -2 -3] (abel/inv m [1 2 3])))

      ;; Commutativity
      (is (= (abel/mul m [1 2] [3 4])
             (abel/mul m [3 4] [1 2]))))))

(deftest test-modulo-add-abelian-group
  (testing "Integers modulo n under addition form an abelian group"
    (let [m (ex/->ModuloAddAbelianGroup)]
      ;; Modulo 7 (from type-map)
      (is (= 2 (abel/mul m 5 4)))  ; 5 + 4 = 9 ≡ 2 (mod 7)

      ;; Commutativity
      (is (= (abel/mul m 3 5) (abel/mul m 5 3)))
      (is (= (abel/mul m 2 6) (abel/mul m 6 2))))))

;;; ============================================================================
;;; Symbolic Model Tests
;;; ============================================================================

(deftest test-symbolic-commutative-monoid
  (testing "Symbolic commutative monoid builds expressions"
    (let [m (ex/->SymCommutativeMonoid)]
      (is (model/symbolic-model? m))

      (let [unit-expr (cmon/unit m)]
        (is (model/symbolic-expr? unit-expr))
        (is (= 'unit (:head unit-expr))))

      (let [mul-expr (cmon/mul m 'x 'y)]
        (is (model/symbolic-expr? mul-expr))
        (is (= 'mul (:head mul-expr)))
        (is (= ['x 'y] (:args mul-expr)))))))

(deftest test-symbolic-group
  (testing "Symbolic group builds expressions"
    (let [m (ex/->SymGroup)]
      (is (model/symbolic-model? m))

      (let [inv-expr (grp/inv m 'x)]
        (is (model/symbolic-expr? inv-expr))
        (is (= 'inv (:head inv-expr)))
        (is (= ['x] (:args inv-expr)))))))

(deftest test-symbolic-abelian-group
  (testing "Symbolic abelian group builds expressions"
    (let [m (ex/->SymAbelianGroup)]
      (is (model/symbolic-model? m))

      ;; Nested expressions
      (let [x+y (abel/mul m 'x 'y)
            inv-x (abel/inv m 'x)
            expr (abel/mul m x+y inv-x)]
        (is (model/symbolic-expr? expr))
        (is (= 'mul (:head expr)))
        (is (= x+y (first (:args expr))))
        (is (= inv-x (second (:args expr))))))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(deftest test-model-dispatch-isolation
  (testing "Different models dispatch independently"
    (let [int-add (ex/->IntAddGroup)
          int-abel (ex/->IntAddAbelianGroup)]
      ;; Both compute same result for addition
      (is (= (grp/mul int-add 3 5)
             (abel/mul int-abel 3 5)))

      ;; But have different theories
      (is (= alg/ThGroup (model/theory int-add)))
      (is (= alg/ThAbelianGroup (model/theory int-abel))))))

(deftest test-multiple-models-same-theory
  (testing "Multiple models can implement the same theory"
    (let [nat-add (ex/->NatAddMonoid)
          nat-mul (ex/->NatMulMonoid)]
      ;; Both implement ThCommutativeMonoid
      (is (= alg/ThCommutativeMonoid (model/theory nat-add)))
      (is (= alg/ThCommutativeMonoid (model/theory nat-mul)))

      ;; But have different operations
      (is (= 0 (cmon/unit nat-add)))
      (is (= 1 (cmon/unit nat-mul)))
      (is (= 7 (cmon/mul nat-add 3 4)))
      (is (= 12 (cmon/mul nat-mul 3 4))))))
