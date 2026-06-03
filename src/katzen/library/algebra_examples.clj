(ns katzen.library.algebra-examples
  "Example models for algebraic theories.

  This file demonstrates concrete and symbolic models for:
  - Semigroups
  - Commutative Monoids
  - Groups
  - Abelian Groups"
  (:require [katzen.library.algebra :as alg]
            [katzen.library.algebra.th-semigroup :as semi]
            [katzen.library.algebra.th-commutative-monoid :as cmon]
            [katzen.library.algebra.th-group :as grp]
            [katzen.library.algebra.th-abelian-group :as abel]
            [katzen.model :as model]))

;;; ============================================================================
;;; Semigroup Examples
;;; ============================================================================

(model/definstance NatAddSemigroup alg/ThSemigroup
  {:s-type :nat}

  (S [_model args]
     true)  ; All natural numbers are valid

  (mul [_model args]
       (let [[x y] args]
         (+ x y))))

(model/definstance NatMulSemigroup alg/ThSemigroup
  {:s-type :nat}

  (S [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (* x y))))

(model/definstance StringSemigroup alg/ThSemigroup
  {:s-type :string}

  (S [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (str x y))))

(model/defsymbolic SymSemigroup alg/ThSemigroup
  {:normalize? false})

;;; ============================================================================
;;; Commutative Monoid Examples
;;; ============================================================================

(model/definstance NatAddMonoid alg/ThCommutativeMonoid
  {:m-type :nat}

  (M [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (+ x y)))

  (unit [_model args]
        0))

(model/definstance NatMulMonoid alg/ThCommutativeMonoid
  {:m-type :nat}

  (M [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (* x y)))

  (unit [_model args]
        1))

(model/definstance BoolAndMonoid alg/ThCommutativeMonoid
  {:m-type :boolean}

  (M [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (and x y)))

  (unit [_model args]
        true))

(model/definstance BoolOrMonoid alg/ThCommutativeMonoid
  {:m-type :boolean}

  (M [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (or x y)))

  (unit [_model args]
        false))

(model/defsymbolic SymCommutativeMonoid alg/ThCommutativeMonoid
  {:normalize? false})

;;; ============================================================================
;;; Group Examples
;;; ============================================================================

(model/definstance IntAddGroup alg/ThGroup
  {:g-type :integer}

  (G [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (+ x y)))

  (unit [_model args]
        0)

  (inv [_model args]
       (let [[x] args]
         (- x))))

(model/definstance IntMulUnitsGroup alg/ThGroup
  {:g-type #{-1 1}
   :doc "Multiplicative group of units in integers: {-1, 1}"}

  (G [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (* x y)))

  (unit [_model args]
        1)

  (inv [_model args]
       (let [[x] args]
         x)))  ; 1^-1 = 1, (-1)^-1 = -1

;; Integers modulo n under addition form a group.
;; Example: n=5 gives group {0, 1, 2, 3, 4} under addition mod 5.
(model/definstance ModuloAddGroup alg/ThGroup
  {:g-type :integer
   :modulus 5}  ; Can be parameterized

  (G [model args]
     true)

  (mul [model args]
       (let [[x y] args
             n (get (:type-map model) :modulus 5)]
         (mod (+ x y) n)))

  (unit [_model args]
        0)

  (inv [model args]
       (let [[x] args
             n (get (:type-map model) :modulus 5)]
         (mod (- n x) n))))

(model/defsymbolic SymGroup alg/ThGroup
  {:normalize? false})

;;; ============================================================================
;;; Abelian Group Examples
;;; ============================================================================

(model/definstance IntAddAbelianGroup alg/ThAbelianGroup
  {:g-type :integer}

  (G [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (+ x y)))

  (unit [_model args]
        0)

  (inv [_model args]
       (let [[x] args]
         (- x))))

(model/definstance RatAddAbelianGroup alg/ThAbelianGroup
  {:g-type :ratio}

  (G [_model args]
     true)

  (mul [_model args]
       (let [[x y] args]
         (+ x y)))

  (unit [_model args]
        0)

  (inv [_model args]
       (let [[x] args]
         (- x))))

;; Vectors in R^n under addition form an abelian group.
;; This demonstrates that the theory can model infinite-dimensional structures.
(model/definstance VectorAddAbelianGroup alg/ThAbelianGroup
  {:g-type :vector}

  (G [_model args]
     true)

  (mul [_model args]
       (let [[v w] args]
         (mapv + v w)))

  (unit [_model args]
        [])  ; Empty vector as zero

  (inv [_model args]
       (let [[v] args]
         (mapv - v))))

;; Integers modulo n under addition form an abelian group.
(model/definstance ModuloAddAbelianGroup alg/ThAbelianGroup
  {:g-type :integer
   :modulus 7}

  (G [_model args]
     true)

  (mul [model args]
       (let [[x y] args
             n (get (:type-map model) :modulus 7)]
         (mod (+ x y) n)))

  (unit [_model args]
        0)

  (inv [model args]
       (let [[x] args
             n (get (:type-map model) :modulus 7)]
         (mod (- n x) n))))

(model/defsymbolic SymAbelianGroup alg/ThAbelianGroup
  {:normalize? false})

;;; ============================================================================
;;; Usage Examples
;;; ============================================================================

(comment
  ;; Semigroups
  (let [nat-add (->NatAddSemigroup)
        str-sem (->StringSemigroup)]
    (semi/mul nat-add 3 5)           ; => 8
    (semi/mul str-sem "hello" "world")) ; => "helloworld"

  ;; Commutative Monoids
  (let [nat-add (->NatAddMonoid)
        bool-and (->BoolAndMonoid)]
    (cmon/unit nat-add)               ; => 0
    (cmon/mul nat-add 3 5)            ; => 8
    (cmon/mul nat-add (cmon/unit nat-add) 5) ; => 5
    (cmon/mul bool-and true false)    ; => false
    (cmon/unit bool-and))             ; => true

  ;; Groups
  (let [int-add (->IntAddGroup)
        mod5 (->ModuloAddGroup)]
    (grp/unit int-add)               ; => 0
    (grp/inv int-add 5)              ; => -5
    (grp/mul int-add 3 (grp/inv int-add 3)) ; => 0
    (grp/mul mod5 3 4)               ; => 2 (mod 5)
    (grp/inv mod5 3))                ; => 2 (because 3 + 2 = 5 ≡ 0)

  ;; Abelian Groups
  (let [vec-add (->VectorAddAbelianGroup)]
    (abel/mul vec-add [1 2 3] [4 5 6]) ; => [5 7 9]
    (abel/inv vec-add [1 2 3])         ; => [-1 -2 -3]
    (abel/unit vec-add))               ; => []

  ;; Symbolic models
  (let [sym (->SymGroup)]
    (grp/mul sym 'x 'y)               ; => SymbolicExpr{:head mul, :args [x y]}
    (grp/inv sym 'x)                  ; => SymbolicExpr{:head inv, :args [x]}
    (grp/unit sym)))                  ; => SymbolicExpr{:head unit, :args []}
