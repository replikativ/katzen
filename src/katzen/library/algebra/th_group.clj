(ns katzen.library.algebra.th-group
  "Theory of groups: monoid with inverse operation.

  A group (G, •, e, ⁻¹) consists of:
  - A carrier set G
  - A binary operation • : G × G → G
  - An identity element e : G
  - An inverse operation ⁻¹ : G → G
  - Associativity: (x • y) • z = x • (y • z)
  - Left identity: e • x = x
  - Right identity: x • e = x
  - Left inverse: x⁻¹ • x = e
  - Right inverse: x • x⁻¹ = e

  Examples:
  - Integers under addition (inverse = negation, identity = 0)
  - Non-zero rationals under multiplication (inverse = reciprocal, identity = 1)
  - Permutations under composition (inverse = reverse permutation)
  - Matrices with non-zero determinant under multiplication
  - Rotations in 3D space under composition
  - Symmetries of geometric objects"
  (:require [katzen.theory :refer [deftheory]]))

(deftheory ThGroup
  (type G)

  (term mul
        :args [x G, y G]
        :ret G)

  (term unit
        :ret G)

  (term inv
        :args [x G]
        :ret G)

  (axiom associativity
         :ctx [x G, y G, z G]
         (= (mul (mul x y) z)
            (mul x (mul y z))))

  (axiom left-unit
         :ctx [x G]
         (= (mul (unit) x) x))

  (axiom right-unit
         :ctx [x G]
         (= (mul x (unit)) x))

  (axiom left-inverse
         :ctx [x G]
         (= (mul (inv x) x) (unit)))

  (axiom right-inverse
         :ctx [x G]
         (= (mul x (inv x)) (unit))))
