(ns katzen.acset.migration-test
  "Tests for Δ-migration of ACSets along schema morphisms.

   These tests target the VectorACSet backend — same convention as the
   homomorphism backtracker tests. A datahike-aware variant lives under
   :test-ansatz or can be added when broader cross-backend coverage is
   needed."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.migration :as m]
            [katzen.acset.morphism :as am]))

;; ============================================================================
;; Builders
;; ============================================================================

(defn- triangle
  "Cycle 1 → 2 → 3 → 1."
  []
  (let [[g _] (a/add-vertices (a/graph) 3)
        [g _] (a/add-edge g 1 2)
        [g _] (a/add-edge g 2 3)
        [g _] (a/add-edge g 3 1)]
    g))

(defn- path-3
  "1 → 2 → 3."
  []
  (let [[g _] (a/add-vertices (a/graph) 3)
        [g _] (a/add-edge g 1 2)
        [g _] (a/add-edge g 2 3)]
    g))

(defn- edge-list
  "Return the list of [src tgt] pairs for every edge."
  [g]
  (mapv (fn [e] [(a/src g e) (a/tgt g e)]) (a/edges g)))

;; ============================================================================
;; Identity migration
;; ============================================================================

(deftest test-identity-migration-empty
  (testing "Δ_id on empty graph yields empty graph"
    (let [g' (m/migrate m/IdGraph (a/graph))]
      (is (= 0 (a/nv g')))
      (is (= 0 (a/ne g'))))))

(deftest test-identity-migration-triangle
  (testing "Δ_id on a triangle yields the same triangle (structurally)"
    (let [g  (triangle)
          g' (m/migrate m/IdGraph g)]
      (is (= (a/nv g) (a/nv g')))
      (is (= (a/ne g) (a/ne g')))
      (is (= (edge-list g) (edge-list g'))))))

(deftest test-identity-migration-path
  (testing "Δ_id on a path yields the same path"
    (let [g  (path-3)
          g' (m/migrate m/IdGraph g)]
      (is (= (edge-list g) (edge-list g'))))))

;; ============================================================================
;; Edge-reversal migration (Δ_op)
;; ============================================================================

(deftest test-op-migration-reverses-edges
  (testing "Δ_op on a triangle reverses every edge"
    (let [g  (triangle)
          g' (m/migrate m/OpGraph g)]
      (is (= 3 (a/nv g')))
      (is (= 3 (a/ne g')))
      (is (= [[2 1] [3 2] [1 3]] (edge-list g'))
          "Edges reversed: 1→2 becomes 2→1, etc."))))

(deftest test-op-migration-involution
  (testing "Δ_op ∘ Δ_op recovers the original graph (op is an involution)"
    (let [g    (triangle)
          gop  (m/migrate m/OpGraph g)
          gopop (m/migrate m/OpGraph gop)]
      (is (= (edge-list g) (edge-list gopop))))))

(deftest test-op-migration-path
  (testing "Δ_op on path 1→2→3 yields 2→1, 3→2 (i.e. path 3→2→1)"
    (let [g  (path-3)
          g' (m/migrate m/OpGraph g)]
      (is (= #{[2 1] [3 2]} (set (edge-list g')))))))

;; ============================================================================
;; Partial-morphism handling
;; ============================================================================

(deftest test-partial-morphism-survives-migration
  (testing "Edge with src set but tgt unset migrates with the same gap"
    (let [[g _] (a/add-vertices (a/graph) 2)
          [g e] (a/add-part g :E)
          g     (a/set-subpart g :src e 1)        ; tgt deliberately unset
          gop   (m/migrate m/OpGraph g)]
      (is (= 1 (a/ne gop)) "still one edge in the migrated graph")
      ;; Op swaps src and tgt — original src=1, tgt=nil should become
      ;; src=nil, tgt=1 after migration.
      (let [e' (first (a/edges gop))]
        (is (nil?  (a/src gop e')))
        (is (= 1   (a/tgt gop e')))))))

;; ============================================================================
;; Validation
;; ============================================================================

(deftest test-validate-missing-ob-map
  (testing "SchemaMorphism missing an ob-map entry throws on validation"
    (let [bad (m/schema-morphism 'Bad a/SchGraph a/SchGraph
                                 {:V :V}             ; missing :E
                                 {:src [:src] :tgt [:tgt]})]
      (is (thrown-with-msg? Exception #"missing ob-map"
                            (m/migrate bad (triangle)))))))

(deftest test-validate-missing-hom-map
  (testing "SchemaMorphism missing a hom-map entry throws on validation"
    (let [bad (m/schema-morphism 'Bad a/SchGraph a/SchGraph
                                 {:V :V :E :E}
                                 {:src [:src]})]      ; missing :tgt
      (is (thrown-with-msg? Exception #"missing hom-map"
                            (m/migrate bad (triangle)))))))

(deftest test-validate-path-discontinuous
  (testing "A hom-map path whose first step doesn't start at F(dom) throws"
    ;; :src expects to start at :E in the codom; here we route it through
    ;; the V-targeted ones in an impossible order.
    (let [bad (m/schema-morphism 'Bad a/SchGraph a/SchGraph
                                 {:V :V :E :E}
                                 {:src [:src :src]    ; src lands at V; can't apply src again
                                  :tgt [:tgt]})]
      (is (thrown-with-msg? Exception #"discontinuous"
                            (m/migrate bad (triangle)))))))

(deftest test-validate-wrong-endpoint
  (testing "Hom-map path landing on the wrong F(codom) throws"
    ;; :src in C is :E → :V, so F(:src) must end at F(:V) = :V.
    ;; Path [] (identity) on F(:E) = :E ends at :E, not :V → error.
    (let [bad (m/schema-morphism 'Bad a/SchGraph a/SchGraph
                                 {:V :V :E :E}
                                 {:src []             ; identity on :E
                                  :tgt [:tgt]})]
      (is (thrown-with-msg? Exception #"does not land"
                            (m/migrate bad (triangle)))))))

(deftest test-schema-mismatch
  (testing "Source ACSet's schema must match F's codom"
    ;; Make a SchemaMorphism whose codom is *not* SchGraph, then try to
    ;; migrate a SchGraph instance — the precondition should fire.
    (let [other-schema {:name 'Other :objects [:V] :homs [] :attr-types [] :attrs []}
          bad (m/schema-morphism 'Bad a/SchGraph other-schema
                                 {:V :V} {})]
      (is (thrown-with-msg? Exception #"Schema mismatch"
                            (m/migrate bad (triangle)))))))

;; ============================================================================
;; Sanity: backend is preserved by default
;; ============================================================================

(deftest test-default-target-matches-source-backend
  (testing "Migrating a VectorACSet yields a VectorACSet by default"
    (let [g  (triangle)
          g' (m/migrate m/IdGraph g)]
      (is (instance? katzen.acset.VectorACSet g')))))

;; ============================================================================
;; Morphism-level migration (Δ_F lifted to ACSet morphisms)
;; ============================================================================
;;
;; The categorical claim is that Δ_F is a functor C-Set(D) → C-Set(C).
;; Concretely: if φ: X → X' is a natural morphism on D, then
;; (migrate-morphism F φ) is a natural morphism on C between the
;; Δ-migrated endpoints. We verify naturality on the result directly.

(defn- rotation-on
  "Cyclic rotation 1→2→3→1 lifted consistently to edges."
  [g]
  (am/acset-morphism g g
                     {:V {1 2 2 3 3 1}
                      :E {1 2 2 3 3 1}}))

(deftest test-migrate-morphism-on-identity-migration
  (testing "Δ_id(φ) ≅ φ — identity migration of a morphism is structurally
            the same morphism"
    (let [g     (triangle)
          phi   (rotation-on g)
          phi'  (m/migrate-morphism m/IdGraph phi)]
      (is (am/natural? phi'))
      (is (= (:components phi) (:components phi'))))))

(deftest test-migrate-morphism-preserves-naturality-under-op
  (testing "Δ_op of a natural morphism is natural on the reversed-edge schema"
    (let [g    (triangle)
          phi  (rotation-on g)
          phi' (m/migrate-morphism m/OpGraph phi)]
      (is (am/natural? phi)
          "the source morphism is natural in C")
      (is (am/natural? phi')
          "and Δ_op extends it naturally to the reversed-edge category")
      (is (= 3 (a/ne (:src phi')))
          "edge count preserved"))))

(deftest test-migrate-morphism-of-identity-is-identity
  (testing "Δ_F(id_X) = id_{Δ_F X}"
    (let [g     (triangle)
          id-g  (am/identity-morphism g)
          mid   (m/migrate-morphism m/OpGraph id-g)]
      (is (am/natural? mid))
      ;; Components map every part to itself (after the part-id translation).
      (doseq [O [:V :E]]
        (let [comp (get (:components mid) O)]
          (is (every? (fn [[p q]] (= p q)) comp)
              (str "object " O " should map every part to itself, got " comp)))))))
