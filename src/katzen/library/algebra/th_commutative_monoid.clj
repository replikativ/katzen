(ns katzen.library.algebra.th-commutative-monoid
  "Theory of commutative monoids: monoid with commutative multiplication.

  A commutative monoid (M, •, e) consists of:
  - A carrier set M
  - A binary operation • : M × M → M
  - An identity element e : M
  - Associativity: (x • y) • z = x • (y • z)
  - Left identity: e • x = x
  - Right identity: x • e = x
  - Commutativity: x • y = y • x

  Examples:
  - Natural numbers under addition (identity = 0)
  - Natural numbers under multiplication (identity = 1)
  - Boolean values under AND (identity = true)
  - Boolean values under OR (identity = false)
  - Sets under union (identity = ∅)
  - Sets under intersection (identity = universe)"
  (:require [katzen.theory :refer [deftheory]]))

(deftheory ThCommutativeMonoid
  (type M)

  (term mul
    :args [x M, y M]
    :ret M)

  (term unit
    :ret M)

  (axiom associativity
    :ctx [x M, y M, z M]
    (= (mul (mul x y) z)
       (mul x (mul y z))))

  (axiom left-unit
    :ctx [x M]
    (= (mul (unit) x) x))

  (axiom right-unit
    :ctx [x M]
    (= (mul x (unit)) x))

  (axiom commutativity
    :ctx [x M, y M]
    (= (mul x y) (mul y x))))
