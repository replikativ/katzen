(ns katzen.petri.migration-test
  "Tests for PetriDynamics migration: validates that the schema-migration
   layer composes cleanly with the compile framework. Two concrete
   schema morphisms exercise the rate-pullback machinery:

   - IdPetri: identity on SchPetri. Migrating a PetriDynamics through
     IdPetri must yield an equivalent dynamical system — same final
     state, same peak.

   - OpPetri: swap input/output arcs. Reverses every reaction;
     integrating from an end-of-SIR state should run backwards into
     the start-of-SIR state."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset.migration :as mig]
            [katzen.compile.core :as cc]
            [katzen.petri :as p]
            [katzen.petri.migration :as pm]
            [raster.ode :as ode]))

;; ============================================================================
;; Schema morphisms on SchPetri
;; ============================================================================

(def IdPetri
  "Identity migration on SchPetri."
  (mig/schema-morphism 'IdPetri p/SchPetri p/SchPetri
                       {:S :S :T :T :I :I :O :O}
                       {:is [:is] :it [:it] :os [:os] :ot [:ot]}))

(def OpPetri
  "Reverse every reaction: swap which arcs count as inputs vs outputs.
   F sends C's :I object onto D's :O and C's :O onto D's :I; the
   underlying parts are the same, but their roles are reversed."
  (mig/schema-morphism 'OpPetri p/SchPetri p/SchPetri
                       {:S :S :T :T :I :O :O :I}
                       {:is [:os] :it [:ot] :os [:is] :ot [:it]}))

;; ============================================================================
;; Builders
;; ============================================================================

(defn- sir-dyn [beta-N gamma]
  (let [n     (p/petri)
        [n s]   (p/add-species n)
        [n i]   (p/add-species n)
        [n _r]  (p/add-species n)
        [n inf] (p/add-transition n)
        [n rec] (p/add-transition n)
        [n _]   (p/add-input  n s inf)
        [n _]   (p/add-input  n i inf)
        [n _]   (p/add-output n i inf)
        [n _]   (p/add-output n i inf)
        [n _]   (p/add-input  n i rec)
        [n _]   (p/add-output n 3 rec)]
    (p/petri-dynamics n {inf beta-N rec gamma})))

(defn- final [sol] (vec (last (:us sol))))

(defn- integrate [rhs u0 tspan]
  (ode/solve (ode/tsit5) (ode/ode-problem rhs (double-array u0)
                                          (first tspan) (second tspan)) 1.0))

;; ============================================================================
;; Identity migration preserves dynamics
;; ============================================================================

(deftest test-identity-migration-preserves-trajectory
  (testing "Migrating SIR through IdPetri then compiling integrates to the
            same trajectory as compiling the original directly"
    (let [beta-N (/ 0.3 1000.0)
          gamma 0.1
          T 100.0
          orig (sir-dyn beta-N gamma)
          migrated (pm/migrate-dynamics IdPetri orig)
          rhs-o (cc/compile-rhs orig)
          rhs-m (cc/compile-rhs migrated)
          u0 [999.0 1.0 0.0]
          sol-o (integrate rhs-o u0 [0.0 T])
          sol-m (integrate rhs-m u0 [0.0 T])
          fo (final sol-o)
          fm (final sol-m)]
      (dotimes [k 3]
        (let [rel (/ (Math/abs (- (nth fo k) (nth fm k)))
                     (max 1e-6 (Math/abs (nth fo k))))]
          (is (< rel 1e-10)
              (str "slot " k ": orig=" (nth fo k) " migrated=" (nth fm k))))))))

(deftest test-identity-migration-preserves-rates
  (testing "After IdPetri migration, the rate map keys map 1-to-1 to the
            original transitions (modulo the part-id translation)"
    (let [orig     (sir-dyn 0.001 0.1)
          migrated (pm/migrate-dynamics IdPetri orig)]
      (is (= (set (vals (:rates orig))) (set (vals (:rates migrated))))
          "the rate values themselves are preserved"))))

;; ============================================================================
;; OpPetri reverses every reaction
;; ============================================================================

(deftest test-op-petri-swaps-substrate-and-product-roles
  (testing "After OpPetri migration, what were inputs (substrates) are now
            outputs (products) and vice versa — the reactions go the
            opposite direction"
    (let [orig     (sir-dyn 0.001 0.1)
          migrated (pm/migrate-dynamics OpPetri orig)
          orig-in   (p/in-multiplicity  (:net orig))
          orig-out  (p/out-multiplicity (:net orig))
          mig-in    (p/in-multiplicity  (:net migrated))
          mig-out   (p/out-multiplicity (:net migrated))]
      ;; The migrated net's :I parts correspond to the original's :O parts
      ;; (and vice versa). So the multiplicity counts swap: what was input
      ;; multiplicity in orig is output multiplicity in migrated.
      (is (= (vals orig-in)  (vals mig-out)))
      (is (= (vals orig-out) (vals mig-in))))))

(deftest test-op-petri-rates-still-attached-to-transitions
  (testing "OpPetri keeps transition identity intact (the :T bijection is
            identity), so rates transfer unchanged"
    (let [orig     (sir-dyn 0.001 0.1)
          migrated (pm/migrate-dynamics OpPetri orig)]
      (is (= (sort (vals (:rates orig)))
             (sort (vals (:rates migrated))))))))

;; ============================================================================
;; The protocol pipeline still works after migration
;; ============================================================================

(deftest test-migrated-dynamics-is-rasterc-compilable
  (testing "The migrated value satisfies the RasterCompilable protocol with
            no additional wrapping"
    (let [migrated (pm/migrate-dynamics IdPetri (sir-dyn 0.001 0.1))]
      (is (cc/compilable? migrated))
      (is (instance? katzen.compile.core.StateLayout
                     (cc/layout-of migrated))))))
