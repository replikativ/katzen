(ns katzen.migrate
  "The unified front door for functorial data migration.

   A schema morphism `F` induces Δ-migration that applies uniformly to
   several kinds of object: an ACSet, an ACSet morphism, or a bundled
   dynamical system (a `PetriDynamics`). Catlab spells all of these
   `migrate`, overloaded on the second argument; katzen implemented them
   as `acset.migration/migrate`, `acset.migration/migrate-morphism`, and
   `petri.migration/migrate-dynamics`. This multimethod is the single
   `migrate` over them, dispatching on what is being migrated. The named
   functions remain callable directly."
  (:refer-clojure :exclude [])
  (:require [katzen.acset :as a]
            [katzen.acset.migration :as mig]
            [katzen.acset.morphism :as _am]   ; load for the ACSetMorphism class
            [katzen.petri :as _p]              ; load for the PetriDynamics class
            [katzen.petri.migration :as pmig])
  (:import [katzen.acset.morphism ACSetMorphism]
           [katzen.petri PetriDynamics]))

(defn- target-kind
  "What is being migrated — dispatches `migrate` on the second argument."
  [x]
  (cond
    (a/acset? x)                :acset
    (instance? ACSetMorphism x) :morphism
    (instance? PetriDynamics x) :dynamics
    :else (throw (ex-info "migrate: unrecognized migration target"
                          {:value x :type (type x)}))))

(defmulti migrate
  "Δ-migrate `x` along the schema morphism `F`. `x` may be an ACSet
   (optionally with a migration `opts` map), an ACSet morphism, or a
   `PetriDynamics`. Dispatches on `x`."
  (fn [_F x & _] (target-kind x)))

(defmethod migrate :acset [F x & [opts]]
  (if opts (mig/migrate F x opts) (mig/migrate F x)))

(defmethod migrate :morphism [F phi & _] (mig/migrate-morphism F phi))

(defmethod migrate :dynamics [F dyn & _] (pmig/migrate-dynamics F dyn))
