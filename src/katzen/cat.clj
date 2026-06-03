(ns katzen.cat
  "The generic category interface — `compose`, `id`, `dom`, `codom` — over
   every category katzen implements, the way Catlab exposes one `compose` /
   `id` / `dom` / `codom` via multiple dispatch.

   katzen carries each category's morphisms as their own records:
   `FinFunction` (the category FinSet), `ACSetMorphism` (C-Set), and the
   theory-morphism records `TheoryMorphism` / `IdTheoryMap` / `TheoryIncl`
   (the category of GATs). The per-category laws live in `katzen.finset`,
   `katzen.acset.morphism`, `katzen.morphism` and remain callable directly;
   these multimethods are the single front door over them, so a caller
   writes `(compose f g)` regardless of which category `f` lives in — no
   `compose-morphisms` / `cod`-vs-`codom` suffix to remember.

   COMPOSITION CONVENTION: diagrammatic order — the FIRST argument is
   applied first. `(compose f g)` requires `(codom f) = (dom g)` and acts
   as `g(f(x))` (classical `g ∘ f`). Every underlying `compose` already
   agrees on this; the multimethod just makes it uniform."
  (:refer-clojure :exclude [compose])
  (:require [katzen.acset :as a]
            [katzen.acset.morphism :as am]
            [katzen.core :as core]
            [katzen.finset :as fs]
            [katzen.morphism :as tm])
  (:import [katzen.acset.morphism ACSetMorphism]
           [katzen.finset FinFunction]))

;; ============================================================================
;; Classifiers
;; ============================================================================

(defn- morphism-kind
  "Which category's morphism is `m`?"
  [m]
  (cond
    (instance? FinFunction m)   :fin-function
    (instance? ACSetMorphism m) :acset-morphism
    (or (tm/theory-morphism? m)
        (tm/id-theory-map? m)
        (tm/theory-incl? m))    :theory-map
    :else (throw (ex-info "Not a recognized morphism" {:value m :type (type m)}))))

(defn- object-kind
  "Which category's object is `x`? (for `id`, which builds the identity
   morphism on an object)."
  [x]
  (cond
    (fs/fin-set? x) :fin-set
    (a/acset? x)    :acset
    (core/gat? x)   :gat
    :else (throw (ex-info "Not a recognized object" {:value x :type (type x)}))))

;; ============================================================================
;; compose / dom / codom — over morphisms
;; ============================================================================

(defmulti compose
  "Compose two morphisms in diagrammatic order (first argument first).
   Dispatches on the morphism's category. See ns docstring for the
   convention."
  (fn [f _g] (morphism-kind f)))

(defmethod compose :fin-function   [f g] (fs/compose f g))
(defmethod compose :acset-morphism [f g] (am/compose f g))
(defmethod compose :theory-map     [f g] (tm/compose-morphisms f g))

(defmulti dom   "Domain object of a morphism."   morphism-kind)
(defmethod dom :fin-function   [f] (fs/dom f))
(defmethod dom :acset-morphism [f] (:src f))
(defmethod dom :theory-map     [f] (tm/dom f))

(defmulti codom "Codomain object of a morphism." morphism-kind)
(defmethod codom :fin-function   [f] (fs/cod f))
(defmethod codom :acset-morphism [f] (:tgt f))
(defmethod codom :theory-map     [f] (tm/codom f))

;; ============================================================================
;; id — over objects
;; ============================================================================

(defmulti id
  "The identity morphism on an object. Dispatches on the object's category."
  object-kind)

(defmethod id :fin-set [s] (fs/id-function s))
(defmethod id :acset   [x] (am/identity-morphism x))
(defmethod id :gat     [g] (tm/id-theory-map g))
