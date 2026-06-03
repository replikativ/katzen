(ns katzen.uwd.algebras
  "Generic operadic composition over UWDs.

   A UWD is an operad — given an *algebra* for it, the UWD's wiring
   pattern composes per-box values into one composite value.

   Two shapes of algebra appear in Catlab.jl:

   1. **Scalar / multiplicative**: each box maps a port-tuple to a scalar
      (a truth-value, a count, a probability density). The composite
      value is a scalar per junction-assignment, equal to the product of
      box-scalars. This namespace ships the generic `oapply-scalar` for
      this case along with two concrete algebras — `relations` and
      `counting` — covering the canonical examples.

   2. **Categorical / colimit**: each box maps to an *object* (a Petri
      net, an ODE system) and composition is a colimit in that object
      category. Implemented per use case — the SIR Petri example does
      this in `katzen.petri`.

   The scalar-multiplicative shape is enough to cover relational
   programming, weighted-relation queries, and tensor-network-style
   composition over a small alphabet."
  (:require [katzen.uwd :as uwd]))

;; ============================================================================
;; The Algebra protocol
;; ============================================================================

(defprotocol Algebra
  "An algebra for the UWD operad. `oapply` takes a UWD `d` and a
   per-box-value map `box-values` and returns the composite value
   determined by `d`'s wiring pattern."
  (oapply [algebra d box-values]))

;; ============================================================================
;; Scalar / multiplicative algebras
;; ============================================================================
;;
;; Helper for both Relations and Counting: enumerate all junction
;; assignments, evaluate every box's contribution on the projected
;; tuple, fold via the algebra's combiner. The result is a map
;; junction-assignment → scalar.

(defn- enumerate-assignments
  "Lazy seq of all length-n vectors over (range type-size)."
  [n type-size]
  (if (zero? n)
    (list [])
    (for [t (enumerate-assignments (dec n) type-size)
          v (range type-size)]
      (conj t v))))

(defn- box-port-projections
  "Map box-id → vector of junction-ids that the box's ports attach to,
   precomputed so we don't traverse the UWD on every assignment."
  [d]
  (into {} (for [b (uwd/boxes d)] [b (uwd/box-junctions d b)])))

(defn- assignment->box-tuple
  "Given a junction-assignment vector indexed 0..nj-1 (junction-id 1
   is at index 0) and a sequence of junction-ids belonging to a box,
   return the tuple of assigned values in port-order."
  [assignment box-junction-ids]
  (mapv #(get assignment (dec %)) box-junction-ids))

(defn oapply-scalar
  "Generic scalar-multiplicative oapply.

     d            UWD
     box-values   {box-id → value}; value is whatever the algebra interprets
     box-eval     fn (value, tuple) → scalar
     combine      associative binary fn on scalars
     unit         identity element of combine
     project?     fn scalar → boolean; assignments where the combined
                  scalar is `project?`-false are dropped from the result

   Returns a map {junction-assignment → scalar} for every assignment
   whose combined scalar passes `project?`."
  [d box-values type-size box-eval combine unit project?]
  (let [nj          (uwd/njunctions d)
        all-boxes   (uwd/boxes d)
        box-tuples  (box-port-projections d)]
    (into {}
          (keep
           (fn [a]
             (let [score (reduce
                          (fn [acc b]
                            (combine acc
                                     (box-eval (get box-values b)
                                               (assignment->box-tuple a (get box-tuples b)))))
                          unit
                          all-boxes)]
               (when (project? score)
                 [a score]))))
          (enumerate-assignments nj type-size))))

;; ============================================================================
;; RelationsAlgebra
;; ============================================================================
;;
;; A box value is a set of tuples. box-eval returns 1 if the tuple is in
;; the box's relation, 0 otherwise. The composite holds for an assignment
;; iff every box accepts its projected tuple — equivalent to multiplying
;; truth-values with 1·1·…·1 = 1 vs 0 anywhere.

(defrecord RelationsAlgebra [type-size])

(defn relations
  "Algebra for set-valued relations over a junction type of cardinality `n`."
  [n]
  (->RelationsAlgebra n))

(extend-protocol Algebra
  RelationsAlgebra
  (oapply [{:keys [type-size]} d box-values]
    (->> (oapply-scalar d box-values type-size
                        (fn [rel tup] (if (contains? rel tup) 1 0))
                        *
                        1
                        pos?)
         keys
         set)))

;; ============================================================================
;; CountingAlgebra
;; ============================================================================
;;
;; A box value is a multiset of tuples (a map tuple→count). box-eval
;; returns the count for a given tuple. The composite count for an
;; assignment is the product of counts across all boxes — this is the
;; FinSet-span composition, generalizing the relations algebra.

(defrecord CountingAlgebra [type-size])

(defn counting
  "Algebra for FinSet-span/multiset composition over a junction type of
   cardinality `n`."
  [n]
  (->CountingAlgebra n))

(extend-protocol Algebra
  CountingAlgebra
  (oapply [{:keys [type-size]} d box-values]
    (oapply-scalar d box-values type-size
                   (fn [ms tup] (get ms tup 0))
                   *
                   1
                   pos?)))
