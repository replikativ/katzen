(ns katzen.library.algebra
  "Standard algebraic theories: semigroups, monoids, groups, rings, fields.

  This library provides GAT definitions for common algebraic structures,
  following the hierarchy:

  Semigroup → Monoid → Group → Abelian Group
           ↘ Commutative Monoid ↗

  Each theory can be instantiated with concrete models (like integers, matrices)
  or symbolic models (for algebraic manipulation).

  ## Usage

  Theories are defined in separate namespaces to avoid name collisions.
  Import theory namespaces with aliases for operation access:

  ```clojure
  (require '[katzen.library.algebra :as alg])              ; Theory definitions
  (require '[katzen.library.algebra.th-group :as grp])     ; Group operations
  (require '[katzen.model :as model])

  ;; Define a model
  (model/definstance IntAddGroup alg/ThGroup
    {:g-type :integer}
    (G [_model args] true)
    (mul [_model args] (let [[x y] args] (+ x y)))
    (unit [_model args] 0)
    (inv [_model args] (let [[x] args] (- x))))

  ;; Use operations from theory namespace
  (def m (->IntAddGroup))
  (grp/mul m 3 5)   ; => 8
  (grp/unit m)      ; => 0
  (grp/inv m 5)     ; => -5
  ```

  ## Theory Namespaces

  - `katzen.library.algebra.th-semigroup` - ThSemigroup theory and operations
  - `katzen.library.algebra.th-commutative-monoid` - ThCommutativeMonoid theory and operations
  - `katzen.library.algebra.th-group` - ThGroup theory and operations
  - `katzen.library.algebra.th-abelian-group` - ThAbelianGroup theory and operations"
  (:require [katzen.library.algebra.th-semigroup :as semi]
            [katzen.library.algebra.th-commutative-monoid :as cmon]
            [katzen.library.algebra.th-group :as grp]
            [katzen.library.algebra.th-abelian-group :as abel]))

;; Re-export theories for convenient access
(def ThSemigroup semi/ThSemigroup)
(def ThCommutativeMonoid cmon/ThCommutativeMonoid)
(def ThGroup grp/ThGroup)
(def ThAbelianGroup abel/ThAbelianGroup)
