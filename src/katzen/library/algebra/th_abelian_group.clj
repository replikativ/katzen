(ns katzen.library.algebra.th-abelian-group
  "Theory of abelian (commutative) groups: group with commutative multiplication.

  An abelian group (G, •, e, ⁻¹) consists of:
  - A carrier set G
  - A binary operation • : G × G → G
  - An identity element e : G
  - An inverse operation ⁻¹ : G → G
  - Associativity: (x • y) • z = x • (y • z)
  - Left identity: e • x = x
  - Right identity: x • e = x
  - Left inverse: x⁻¹ • x = e
  - Right inverse: x • x⁻¹ = e
  - Commutativity: x • y = y • x

  Named after Niels Henrik Abel, abelian groups are fundamental in:
  - Linear algebra (vector spaces are abelian groups under addition)
  - Number theory (integers modulo n)
  - Topology (fundamental groups of certain spaces)
  - Algebraic geometry

  Examples:
  - Integers under addition
  - Rational numbers under addition
  - Real numbers under addition
  - Complex numbers under addition
  - Vectors in Rⁿ under addition
  - Integers modulo n under addition"
  (:require [katzen.theory :refer [deftheory]]
            [katzen.library.algebra.th-group]))

;; ThAbelianGroup is just ThGroup + commutativity. Inheritance via `using`
;; pulls in the full group structure (sort G, mul, unit, inv, all 5 axioms).
(deftheory ThAbelianGroup
  (using katzen.library.algebra.th-group/ThGroup)

  (axiom commutativity
         :ctx [x G, y G]
         (= (mul x y) (mul y x))))
