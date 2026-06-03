(ns katzen.petri.migration
  "Migration-aware constructors for PetriDynamics.

   Given a schema morphism F: SchPetri-C → SchPetri-D and a PetriDynamics
   `dyn` on SchPetri-D, `migrate-dynamics F dyn` returns a PetriDynamics
   on SchPetri-C whose underlying net is the Δ-migrated net and whose
   rate constants are pulled back through F.

   The rate pullback is automatic: every C-transition t corresponds via
   the migration bijection to some D-transition F-of(t), and we set
   `rates_Y(t) = rates_X(F-of(t))`. No user input on rate translation
   is needed.

   This is the migration-side of the compile pipeline: because
   `PetriDynamics` implements `katzen.compile.core/RasterCompilable`,
   the migrated value compiles to a typed raster RHS through the
   single existing driver. Adding migration didn't require any
   changes to the protocol or the driver.

   The same pattern applies to other categorical objects that bundle
   an ACSet with auxiliary parameters (ReactionDynamics, etc.) — port
   per concept by following this template."
  (:require [katzen.acset.migration :as mig]
            [katzen.petri :as p]))

(defn migrate-dynamics
  "Δ-migration of a PetriDynamics along a schema morphism `F`.

   `F` must map a SchPetri-shaped C onto D in a way that's well-formed
   for the standard Petri schema — specifically the :T object must
   map to D's :T object so we can pull back rates per transition."
  [F dyn]
  (let [{:keys [net rates]} dyn
        {:keys [result bijection]} (mig/migrate* F net)
        ;; The :T bijection says: y-transition (in result) → x-transition (in net).
        t-bij  (:y->x (get bijection :T))
        rates' (into {} (for [[y x] t-bij]
                          [y (get rates x 0.0)]))]
    (p/petri-dynamics result rates')))
