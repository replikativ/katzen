(ns katzen.acset.morphism-test
  "Tests for ACSet morphisms and the naturality check.

   Three flavors of coverage:

   1. Hand-built morphisms (identity, rotation, deliberate non-natural)
      exercise the constructor + check.
   2. Composition properties: identity composition, associativity check.
   3. Connection to homomorphism search: every result of
      `katzen.acset.homomorphism/homomorphisms` lifts to a natural
      morphism — this is the structural guarantee the backtracker is
      supposed to give us, made explicit."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.homomorphism :as hom]
            [katzen.acset.morphism :as m]))

;; ============================================================================
;; Builders
;; ============================================================================

(defn- triangle
  "1 → 2 → 3 → 1"
  []
  (let [[g _] (a/add-vertices (a/graph) 3)
        [g _] (a/add-edge g 1 2)
        [g _] (a/add-edge g 2 3)
        [g _] (a/add-edge g 3 1)]
    g))

(defn- single-edge []
  (let [[g _] (a/add-vertices (a/graph) 2)
        [g _] (a/add-edge g 1 2)]
    g))

;; ============================================================================
;; Construction + validation
;; ============================================================================

(deftest test-construction-rejects-missing-component
  (testing "Constructor throws when a component is missing for an object"
    (is (thrown-with-msg? Exception #"component missing"
                          (m/acset-morphism (triangle) (triangle)
                                            {:V {1 1 2 2 3 3}})))))

(deftest test-construction-rejects-undefined-on-part
  (testing "Constructor throws when a component skips a src part"
    (is (thrown-with-msg? Exception #"undefined on a src part"
                          (m/acset-morphism (triangle) (triangle)
                                            {:V {1 1 2 2}    ; missing part 3
                                             :E {1 1 2 2 3 3}})))))

(deftest test-construction-rejects-non-tgt-image
  (testing "Constructor throws when an image value isn't a tgt part"
    (is (thrown-with-msg? Exception #"not a tgt part"
                          (m/acset-morphism (triangle) (triangle)
                                            {:V {1 1 2 2 3 99}    ; 99 not in tgt
                                             :E {1 1 2 2 3 3}})))))

(deftest test-construction-rejects-mismatched-schema
  (testing "Constructor throws when src and tgt have different schemas"
    (let [other-schema {:name 'X :objects [:Z] :homs [] :attr-types [] :attrs []}
          weird (a/vector-acset other-schema)]
      (is (thrown-with-msg? Exception #"share a schema"
                            (m/acset-morphism (triangle) weird {}))))))

;; ============================================================================
;; Naturality
;; ============================================================================

(deftest test-identity-is-natural
  (testing "id_G is natural by construction for any G"
    (is (m/natural? (m/identity-morphism (triangle))))
    (is (m/natural? (m/identity-morphism (single-edge))))
    (is (m/natural? (m/identity-morphism (a/graph))))))

(deftest test-rotation-of-triangle-is-natural
  (testing "v ↦ v+1 (mod 3) extended consistently to edges is natural"
    (let [g (triangle)
          phi (m/acset-morphism g g
                                {:V {1 2 2 3 3 1}
                                 :E {1 2 2 3 3 1}})]
      (is (m/natural? phi)))))

(deftest test-non-natural-fails-and-reports
  (testing "A deliberately broken square is caught with a useful diagnostic"
    (let [g (triangle)
          phi (m/acset-morphism g g
                                {:V {1 2 2 2 3 3}    ; 1 mapped to 2, but edges stay id
                                 :E {1 1 2 2 3 3}})
          fails (m/naturality-failures phi)]
      (is (false? (m/natural? phi)))
      (is (= 2 (count fails)) "two squares fail: :src on edge 1, :tgt on edge 3")
      (let [f (first fails)]
        (is (#{:src :tgt} (:hom f)))
        (is (some? (:src-part f)))))))

(deftest test-check-natural-bang-throws
  (testing "check-natural! returns the morphism on success, throws on failure"
    (let [id (m/identity-morphism (triangle))]
      (is (identical? id (m/check-natural! id))))
    (let [bad (m/acset-morphism (triangle) (triangle)
                                {:V {1 2 2 2 3 3} :E {1 1 2 2 3 3}})]
      (is (thrown-with-msg? Exception #"not natural"
                            (m/check-natural! bad))))))

;; ============================================================================
;; Composition
;; ============================================================================

(deftest test-identity-composition
  (testing "id ∘ id = id"
    (let [g (triangle)
          id (m/identity-morphism g)
          composed (m/compose id id)]
      (is (m/natural? composed))
      (is (= (:components id) (:components composed))))))

(deftest test-compose-rejects-mismatched-endpoints
  (testing "compose throws when tgt(φ) is not src(ψ)"
    (let [g1 (triangle)
          g2 (triangle)
          phi (m/identity-morphism g1)
          psi (m/identity-morphism g2)]
      (is (thrown-with-msg? Exception #"tgt.*must equal src"
                            (m/compose phi psi))))))

(deftest test-composition-of-naturals-is-natural
  (testing "Two rotations on the triangle compose to a natural morphism"
    (let [g (triangle)
          rotate (m/acset-morphism g g
                                   {:V {1 2 2 3 3 1} :E {1 2 2 3 3 1}})
          rot2 (m/compose rotate rotate)]
      (is (m/natural? rot2))
      ;; rotate twice: 1→3, 2→1, 3→2
      (is (= {1 3 2 1 3 2} (get-in rot2 [:components :V]))))))

;; ============================================================================
;; Connection to homomorphism search
;; ============================================================================

(deftest test-backtracker-results-lift-to-natural-morphisms
  (testing "Every homomorphism returned by the backtracker is, by construction,
            a natural morphism — the structural guarantee made explicit"
    (let [probe (single-edge)
          tgt   (triangle)
          homs  (hom/homomorphisms probe tgt)
          morphs (map #(m/from-flat-components probe tgt %) homs)]
      (is (= 3 (count morphs)) "triangle has 3 edges")
      (is (every? m/natural? morphs)))))

(deftest test-from-flat-components-reshapes-correctly
  (testing "from-flat-components produces an equivalent nested-components morphism"
    (let [g (single-edge)
          tgt (triangle)
          flat-comp {[:V 1] 1 [:V 2] 2 [:E 1] 1}
          phi (m/from-flat-components g tgt flat-comp)]
      (is (= {1 1 2 2} (get-in phi [:components :V])))
      (is (= {1 1} (get-in phi [:components :E])))
      (is (m/natural? phi)))))
