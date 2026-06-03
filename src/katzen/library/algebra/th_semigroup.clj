(ns katzen.library.algebra.th-semigroup
  "Theory of semigroups: a set with an associative binary operation.

  A semigroup (S, •) consists of:
  - A carrier set S
  - A binary operation • : S × S → S
  - Associativity: (x • y) • z = x • (y • z)

  Examples:
  - Natural numbers under addition
  - Natural numbers under multiplication
  - Strings under concatenation
  - Matrices under multiplication
  - Functions under composition"
  (:require [katzen.theory :refer [deftheory]]))

(deftheory ThSemigroup
  (type S)

  (term mul
    :args [x S, y S]
    :ret S)

  (axiom associativity
    :ctx [x S, y S, z S]
    (= (mul (mul x y) z)
       (mul x (mul y z)))))
