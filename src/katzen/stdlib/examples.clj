(ns katzen.stdlib.examples
  "Example presentations and models using standard theories.

  This module provides concrete examples demonstrating how to use
  the standard theories for modeling real systems."
  (:require [katzen.stdlib.core :as stdlib]
            [katzen.presentation :as pres]))

;;; ============================================================================
;;; Graph Examples
;;; ============================================================================

;; A simple triangle graph: three vertices connected in a cycle.
(pres/defpresentation TriangleGraph stdlib/ThGraph
  (v1 :- V)
  (v2 :- V)
  (v3 :- V)
  (e12 :- E)
  (e23 :- E)
  (e31 :- E))

;; A path graph: vertices connected in a line.
(pres/defpresentation PathGraph stdlib/ThGraph
  (v1 :- V)
  (v2 :- V)
  (v3 :- V)
  (v4 :- V)
  (e1 :- E)
  (e2 :- E)
  (e3 :- E))

;; A star graph: one central vertex connected to all others.
(pres/defpresentation StarGraph stdlib/ThGraph
  (center :- V)
  (v1 :- V)
  (v2 :- V)
  (v3 :- V)
  (v4 :- V)
  (e1 :- E)
  (e2 :- E)
  (e3 :- E)
  (e4 :- E))

;;; ============================================================================
;;; Category Examples
;;; ============================================================================

;; A small finite category with three objects and some morphisms.
(pres/defpresentation FiniteCategory stdlib/ThCategory
  (a :- Ob)
  (b :- Ob)
  (c :- Ob)
  (f :- Hom)
  (g :- Hom)
  (h :- Hom))

;; A monoid viewed as a one-object category.
;; Each monoid element is a morphism from the object to itself.
(pres/defpresentation MonoidAsCategory stdlib/ThCategory
  (star :- Ob)
  (m1 :- Hom)
  (m2 :- Hom)
  (m3 :- Hom))

;;; ============================================================================
;;; Monoid Examples
;;; ============================================================================

;; Free monoid on three generators: strings of x, y, z.
(pres/defpresentation FreeMonoid stdlib/ThMonoid
  (x :- El)
  (y :- El)
  (z :- El))

;; Natural numbers under addition (abstractly).
;; Generators represent numbers, mul represents addition.
(pres/defpresentation NaturalNumbersMonoid stdlib/ThMonoid
  (zero :- El)
  (one :- El)
  (two :- El)
  (three :- El))

;;; ============================================================================
;;; Group Examples
;;; ============================================================================

;; Cyclic group of order 3: rotations of a triangle.
(pres/defpresentation CyclicGroup3 stdlib/ThGroup
  (e :- El)    ; identity
  (r :- El)    ; 120° rotation
  (r2 :- El))  ; 240° rotation

;; Klein four-group: simplest non-cyclic group.
(pres/defpresentation Klein4Group stdlib/ThGroup
  (e :- El)     ; identity
  (a :- El)
  (b :- El)
  (c :- El))

;;; ============================================================================
;;; Example Queries
;;; ============================================================================

(defn example-summary
  "Get summary of an example presentation."
  [pres-var]
  (let [p (var-get pres-var)
        theory (:theory p)]
    {:name (-> pres-var meta :name)
     :theory (:name theory)
     :generators (count (:generators p))
     :equations (count (:equations p))}))

(defn list-examples
  "List all available example presentations."
  []
  {:graphs ['TriangleGraph 'PathGraph 'StarGraph]
   :categories ['FiniteCategory 'MonoidAsCategory]
   :monoids ['FreeMonoid 'NaturalNumbersMonoid]
   :groups ['CyclicGroup3 'Klein4Group]})
