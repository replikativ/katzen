(ns katzen.acset.migration-integration-test
  "Integration tests for the migration layer.

   Three concerns:

   1. **Cross-backend migration.** Verify that vector → datahike and datahike →
      vector both work and produce structurally-equal results. Tests are
      gated on the `:datahike` alias being available; without it the
      gating tests print a notice and pass trivially.

   2. **Functoriality with homomorphism search.** Δ-migration commutes
      with homomorphism enumeration in the obvious sense: the number of
      homomorphisms G → H is invariant under identity migration of either
      argument, and reverses with edge-reversal on both. This catches
      bugs where migration silently drops or reorders parts.

   3. **Demo regression.** Loads the demo namespace and runs each step
      directly, verifying the final ACSets have the expected shape. The
      demo doubles as a non-trivial example, and a regression test on it
      guards against accidental breakage of the public surface."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.graphs :as gg]
            [katzen.acset.homomorphism :as hom]
            [katzen.acset.migration :as m]
            [katzen.test-support :as ts]))

;; ============================================================================
;; Helpers
;; ============================================================================

(def datahike-available? ts/datahike-available?)

(defn- triangle [graph-ctor]
  (let [[g _] (a/add-parts (graph-ctor) :V 3)
        [g _] (a/add-edge g 1 2)
        [g _] (a/add-edge g 2 3)
        [g _] (a/add-edge g 3 1)]
    g))

(defn- edge-set
  "Structural edge view that ignores part-id numbering — useful for cross-
   backend comparison because datahike's eids differ from vector's."
  [g]
  (set (map (fn [e] [(a/src g e) (a/tgt g e)]) (a/edges g))))

(defn- structural-graph
  "{:nv :ne :edges} structural fingerprint, normalized per-vertex by
   relabelling vertices in BFS order. Used for cross-backend equality."
  [g]
  ;; Relabel vertices to 1..nv in the order returned by (parts g :V).
  (let [vs    (vec (a/vertices g))
        relabel (zipmap vs (range 1 (inc (count vs))))]
    {:nv (a/nv g)
     :ne (a/ne g)
     :edges (set (map (fn [e]
                        [(get relabel (a/src g e))
                         (get relabel (a/tgt g e))])
                      (a/edges g)))}))

;; ============================================================================
;; Cross-backend migration
;; ============================================================================

(deftest test-cross-backend-vector-to-datahike
  (if-not datahike-available?
    (ts/skip-notice ":datahike")
    (testing "Migrating a VectorACSet with :target :datahike produces a datahike ACSet
              whose structure matches the source"
      (let [g-vec  (triangle a/graph)
            g-dh   (m/migrate m/IdGraph g-vec {:target :datahike})]
        (is (contains? g-dh :conn) "result is a DatahikeACSet")
        (is (= (structural-graph g-vec) (structural-graph g-dh)))))))

(deftest test-cross-backend-datahike-to-vector
  (if-not datahike-available?
    (ts/skip-notice ":datahike")
    (testing "Migrating a DatahikeACSet with :target :vector produces a vector ACSet
              whose structure matches the source"
      (let [dh-graph-ctor (requiring-resolve 'katzen.acset.datahike/graph)
            g-dh   (triangle dh-graph-ctor)
            g-vec  (m/migrate m/IdGraph g-dh {:target :vector})]
        (is (instance? katzen.acset.VectorACSet g-vec)
            "result is a VectorACSet")
        (is (= (structural-graph g-dh) (structural-graph g-vec)))))))

(deftest test-cross-backend-with-op-migration
  (if-not datahike-available?
    (ts/skip-notice ":datahike")
    (testing "OpGraph migration crosses backends correctly: the cross-backend
              result's structure equals what we'd get from the same migration
              run end-to-end on the source backend."
      (let [g-vec       (triangle a/graph)
            g-dh        (m/migrate m/OpGraph g-vec {:target :datahike})
            g-vec-op    (m/migrate m/OpGraph g-vec)]
        ;; Normalised vertex labelling makes the two backends comparable.
        (is (= (structural-graph g-vec-op) (structural-graph g-dh)))))))

;; ============================================================================
;; Migration commutes with homomorphism enumeration
;; ============================================================================

(deftest test-identity-migration-preserves-hom-count
  (testing "Δ_id preserves the number of homomorphisms from any probe graph"
    (let [g       (triangle a/graph)
          g-id    (m/migrate m/IdGraph g)
          probe   (let [[h _] (a/add-vertices (a/graph) 2)
                        [h _] (a/add-edge h 1 2)]
                    h)]
      (is (= (hom/nhomomorphisms probe g)
             (hom/nhomomorphisms probe g-id))
          "single-edge → triangle: count is the same in original and migrated")
      (is (= (hom/nhomomorphisms g probe)
             (hom/nhomomorphisms g-id probe))
          "triangle → single-edge: count is the same in original and migrated"))))

(deftest test-op-migration-swaps-source-and-target-hom-counts
  (testing "Δ_op X is the edge-reversed graph; hom counts therefore relate
            to bt-vec via a corresponding edge-reverse of the other argument."
    (let [g       (triangle a/graph)
          g-op    (m/migrate m/OpGraph g)
          probe   (let [[h _] (a/add-vertices (a/graph) 2)
                        [h _] (a/add-edge h 1 2)]
                    h)
          probe-op (m/migrate m/OpGraph probe)]
      ;; hom(probe, g) = hom(probe-op, g-op): reversing both sides preserves
      ;; the edge-respecting maps.
      (is (= (hom/nhomomorphisms probe g)
             (hom/nhomomorphisms probe-op g-op))))))

;; ============================================================================
;; Demo regression
;; ============================================================================

(def demo-available?
  "True when the :dev alias (which adds dev/ to the classpath) is in effect."
  (some? (try (require 'notebooks.migration-demo) true
              (catch Throwable _ nil))))

(deftest test-demo-pipeline-shape
  (testing "The migration_demo pipeline produces stable shapes at every stage."
    (if-not demo-available?
      (ts/skip-notice "notebooks.migration-demo")
    (let [network-fn (resolve 'notebooks.migration-demo/road-network)
          network   (network-fn)
          topology  (m/migrate gg/ForgetWeight network)
          reversed  (m/migrate m/OpGraph topology)]
      (is (= 4 (a/nv topology)))
      (is (= 6 (a/ne topology)))
      (is (= a/SchGraph (a/schema topology)))
      (is (nil? (a/subpart topology :weight 1))
          "weight column is gone after ForgetWeight")
      (is (= 4 (a/nv reversed)))
      (is (= 6 (a/ne reversed)))
      (is (= #{[2 1] [3 2] [4 3] [1 4] [3 1] [4 2]}
             (edge-set reversed))
          "edges are exactly the reversal of the original road network")))))
