(ns katzen.stdlib.core
  "Standard theories from category theory and algebra.

  This module provides the core theories that are commonly used:
  - ThGraph: Directed graphs with vertices and edges
  - ThCategory: Categories with objects, morphisms, composition, and identity
  - ThMonoid: Monoids with elements, multiplication, and unit
  - ThGroup: Groups extending monoids with inverses

  These theories are the foundation for most applications of GATs."
  (:require [katzen.theory :as theory]))

;;; ============================================================================
;;; Graph Theory
;;; ============================================================================

;; Theory of directed graphs.
;;
;; A graph consists of:
;; - V: Vertices (nodes)
;; - E: Edges with source and target vertices
;;
;; This is the simplest theory with dependent types.
;; Every edge has a source vertex and a target vertex.
(theory/deftheory ThGraph
  (type V)
  (type E [src V, tgt V]))

;;; ============================================================================
;;; Category Theory
;;; ============================================================================

;; Theory of categories.
;;
;; A category consists of:
;; - Ob: Objects
;; - Hom: Morphisms between objects (with domain and codomain)
;; - compose: Composition of morphisms
;; - id: Identity morphism for each object
;;
;; With axioms:
;; - Associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h)
;; - Left identity: id ∘ f = f
;; - Right identity: f ∘ id = f
(theory/deftheory ThCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))

  (term id
    :ctx [a Ob]
    :ret (Hom a a))

  (axiom assoc
    :ctx [a Ob, b Ob, c Ob, d Ob,
          f (Hom a b), g (Hom b c), h (Hom c d)]
    (= (compose a c d (compose a b c f g) h)
       (compose a b d f (compose b c d g h))))

  (axiom id-left
    :ctx [a Ob, b Ob, f (Hom a b)]
    (= (compose a a b (id a) f) f))

  (axiom id-right
    :ctx [a Ob, b Ob, f (Hom a b)]
    (= (compose a b b f (id b)) f)))

;;; ============================================================================
;;; Symmetric Monoidal Category Theory
;;; ============================================================================

;; A symmetric monoidal category extends a category with:
;; - Tensor product (otimes) on both objects and morphisms
;; - Unit object (munit)
;; - Associator, unitors, and braiding natural isomorphisms
;;
;; The category structure (Ob, Hom, compose, id, assoc, id-left, id-right) is
;; inherited from ThCategory via `using` — modern GATlab.jl-style inheritance.
(theory/deftheory ThSymmetricMonoidalCategory
  (using ThCategory)

  ;; Monoidal structure on objects
  (term otimes
    :ctx [a Ob, b Ob]
    :ret Ob)

  (term munit
    :ret Ob)

  ;; Monoidal structure on morphisms
  (term otimes-hom
    :ctx [a Ob, b Ob, c Ob, d Ob]
    :args [f (Hom a c), g (Hom b d)]
    :ret (Hom (otimes a b) (otimes c d)))

  ;; Structural isomorphisms
  (term associator
    :ctx [a Ob, b Ob, c Ob]
    :ret (Hom (otimes (otimes a b) c) (otimes a (otimes b c))))

  (term left-unitor
    :ctx [a Ob]
    :ret (Hom (otimes munit a) a))

  (term right-unitor
    :ctx [a Ob]
    :ret (Hom (otimes a munit) a))

  (term braid
    :ctx [a Ob, b Ob]
    :ret (Hom (otimes a b) (otimes b a))))

;;; ============================================================================
;;; Schema Theory — foundation for ACSets
;;; ============================================================================

;; A schema is the data of a category C, a discrete category D, and a
;; profunctor Attr : C^op × D → Set. In GAT form: extend ThCategory with
;; AttrType (objects of D) and Attr (the profunctor's elements), plus a
;; compose operation expressing the profunctor action of C on D.
;;
;; To avoid name collision with the homset compose inherited from
;; ThCategory, we call the second compose `compose-attr`. (Catlab.jl
;; reuses `compose` here by relying on Julia multiple dispatch on the
;; argument types, but Clojure protocols don't overload by argtype.)
;;
;; Mirrors Catlab.jl's Theories/Schema.jl, ThSchema <: ThCategory.
(theory/deftheory ThSchema
  (using ThCategory)

  (type AttrType)
  (type Attr [dom Ob, codom AttrType])

  (term compose-attr
    :ctx [a Ob, b Ob, x AttrType]
    :args [f (Hom a b), g (Attr b x)]
    :ret (Attr a x))

  (axiom attr-assoc
    :ctx [a Ob, b Ob, c Ob, x AttrType,
          f (Hom a b), g (Hom b c), h (Attr c x)]
    ;; LHS: f ∘ (g ∘ h) — outer compose-attr is Hom A B × Attr B X → Attr A X
    ;; RHS: (f ∘ g) ∘ h — outer compose-attr is Hom A C × Attr C X → Attr A X
    (= (compose-attr a b x f (compose-attr b c x g h))
       (compose-attr a c x (compose a b c f g) h)))

  (axiom attr-id-left
    :ctx [a Ob, x AttrType, h (Attr a x)]
    (= (compose-attr a a x (id a) h) h)))

;;; ============================================================================
;;; Monoid Theory
;;; ============================================================================

;; Theory of monoids.
;;
;; A monoid consists of:
;; - El: Elements
;; - mul: Binary multiplication operation
;; - unit: Identity element
;;
;; With axioms:
;; - Associativity: (x * y) * z = x * (y * z)
;; - Left identity: unit * x = x
;; - Right identity: x * unit = x
(theory/deftheory ThMonoid
  (type El)

  (term mul
    :ctx [x El, y El]
    :ret El)

  (term unit
    :ret El)

  (axiom assoc
    :ctx [x El, y El, z El]
    (= (mul (mul x y) z)
       (mul x (mul y z))))

  (axiom unit-left
    :ctx [x El]
    (= (mul (unit) x) x))

  (axiom unit-right
    :ctx [x El]
    (= (mul x (unit)) x)))

;;; ============================================================================
;;; Group Theory
;;; ============================================================================

;; Theory of groups.
;;
;; A group extends a monoid with:
;; - inv: Inverse operation
;;
;; With axioms:
;; - Left inverse: inv(x) * x = unit
;; - Right inverse: x * inv(x) = unit
;;
;; Group structure (El, mul, unit + monoid axioms) is inherited from
;; ThMonoid via `using`. Group adds the inverse operation and its laws.
(theory/deftheory ThGroup
  (using ThMonoid)

  (term inv
    :ctx [x El]
    :ret El)

  (axiom inv-left
    :ctx [x El]
    (= (mul (inv x) x) (unit)))

  (axiom inv-right
    :ctx [x El]
    (= (mul x (inv x)) (unit))))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(defn list-theories
  "List all standard theories available in this module."
  []
  ['ThGraph
   'ThCategory
   'ThSchema
   'ThSymmetricMonoidalCategory
   'ThMonoid
   'ThGroup])

(def theory-summaries
  "Summary information for standard theories."
  {'ThGraph
   {:name 'ThGraph
    :description "Directed graphs with vertices and edges"
    :types 2
    :terms 0
    :axioms 0}

   'ThCategory
   {:name 'ThCategory
    :description "Categories with composition and identity"
    :types 2
    :terms 2
    :axioms 3}

   'ThSchema
   {:name 'ThSchema
    :description "Schemas: categories enriched with attribute types and a profunctor"
    :types 4
    :terms 3
    :axioms 5}

   'ThSymmetricMonoidalCategory
   {:name 'ThSymmetricMonoidalCategory
    :description "Symmetric monoidal categories with tensor product and braiding"
    :types 2
    :terms 7
    :axioms 3}

   'ThMonoid
   {:name 'ThMonoid
    :description "Monoids with multiplication and unit"
    :types 1
    :terms 2
    :axioms 3}

   'ThGroup
   {:name 'ThGroup
    :description "Groups with inverse"
    :types 1
    :terms 3
    :axioms 5}})

(defn theory-summary
  "Get a summary of a theory by name."
  [theory-name]
  (get theory-summaries theory-name))
