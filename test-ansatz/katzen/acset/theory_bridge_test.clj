(ns katzen.acset.theory-bridge-test
  "Tests for the ACSet ↔ katzen.theory bridge. Lives under test-ansatz/
   because the verification path through `ansatz/check-morphism!`
   requires the Mathlib store at /var/tmp/ansatz-mathlib."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ansatz.core :as a]
            [katzen.acset :as ga]
            [katzen.acset.graphs :as gg]
            [katzen.acset.migration :as mig]
            [katzen.acset.theory-bridge :as bridge]
            [katzen.test-support :as ts]))

(defn- ansatz-ready? [] (ts/ansatz-ready?))

(use-fixtures :once ts/ensure-ansatz-init!)

;; ============================================================================
;; schema → theory plumbing
;; ============================================================================

(deftest test-bridge-schema-to-theory-basic-shape
  (testing "SchGraph translates to a 2-type, 2-term, 0-axiom GAT"
    (let [thy (bridge/schema->theory ga/SchGraph)]
      (is (= 'SchGraph (:name thy)))
      (is (= 2 (count (:type-constructors thy))))
      (is (= 2 (count (:term-constructors thy))))
      (is (= 0 (count (:axioms thy)))))))

(deftest test-bridge-schema-with-attrs
  (testing "SchWeightedGraph adds an attr-type and an attr term constructor"
    (let [thy (bridge/schema->theory gg/SchWeightedGraph)]
      (is (= 3 (count (:type-constructors thy)))   ; V, E, Weight
          "attr-types become extra type constructors")
      (is (= 3 (count (:term-constructors thy)))   ; src, tgt, weight
          "attrs become extra term constructors"))))

;; ============================================================================
;; Schema morphism → theory morphism
;; ============================================================================

(deftest test-bridge-rejects-empty-path
  (testing "Schemas where a hom maps to an empty path (identity-on-object)
            aren't supported by the v1 bridge"
    (let [dom-thy (bridge/schema->theory gg/SchSymmetricGraph)
          codom-thy (bridge/schema->theory gg/SchSymmetricGraph)
          ;; A morphism that sends :inv to [] (identity on E)
          bad-F (mig/schema-morphism 'BadInvId
                                     gg/SchSymmetricGraph gg/SchSymmetricGraph
                                     {:V :V :E :E}
                                     {:src [:src] :tgt [:tgt] :inv []})]
      ;; Note: validate-schema-morphism! accepts the empty path (identity-on-E).
      ;; The theory-bridge rejects it because there's no identity term to map to.
      (is (thrown-with-msg? Exception #"single-step"
                            (bridge/schema-morphism->theory-morphism
                             bad-F dom-thy codom-thy))))))

;; ============================================================================
;; verify-schema-morphism! through ansatz
;; ============================================================================

(deftest test-verify-id-graph
  (when (ansatz-ready?)
    (testing "IdGraph verifies cleanly (no axioms to discharge)"
      (is (= 'IdGraph (:name (bridge/verify-schema-morphism! mig/IdGraph)))))))

(deftest test-verify-op-graph
  (when (ansatz-ready?)
    (testing "OpGraph (swap src/tgt) verifies cleanly"
      (is (= 'OpGraph (:name (bridge/verify-schema-morphism! mig/OpGraph)))))))

(def SchSymmetricGraphAxiomized
  "SchSymmetricGraph with the involution axiom carried in :axioms."
  (assoc gg/SchSymmetricGraph
         :name 'SchSymGraphAx
         :axioms [{:name 'inv-involution
                   :ctx [{:name 'e :type :E}]
                   :lhs '(inv (inv e))
                   :rhs 'e}]))

(def IdSymAx
  (mig/schema-morphism 'IdSymAx
                       SchSymmetricGraphAxiomized
                       SchSymmetricGraphAxiomized
                       {:V :V :E :E}
                       {:src [:src] :tgt [:tgt] :inv [:inv]}))

(deftest test-verify-identity-on-axiomized-schema
  (when (ansatz-ready?)
    (testing "Identity morphism on an axiomized schema (1 axiom) verifies —
              ansatz discharges the involution obligation via codom-axiom-match"
      (is (= 'IdSymAx (:name (bridge/verify-schema-morphism! IdSymAx)))))))

;; ============================================================================
;; verified-migrate end-to-end
;; ============================================================================

(deftest test-verified-migrate-runs-after-verification
  (when (ansatz-ready?)
    (testing "verified-migrate verifies then migrates a graph"
      (let [g (let [[g _] (ga/add-vertices (ga/graph) 3)
                    [g _] (ga/add-edge g 1 2)
                    [g _] (ga/add-edge g 2 3)]
                g)
            g' (bridge/verified-migrate mig/IdGraph g)]
        (is (= (ga/nv g) (ga/nv g')))
        (is (= (ga/ne g) (ga/ne g')))))))

;; ============================================================================
;; verified-migrate with instance-axiom checking
;; ============================================================================

(defn- good-sym-graph
  "Two vertices, one symmetric edge pair, all axioms satisfied."
  []
  (let [g (ga/vector-acset SchSymmetricGraphAxiomized)
        [g _] (ga/add-parts g :V 2)
        [g [e1 e2]] (ga/add-parts g :E 2)]
    (-> g
        (ga/set-subpart :src e1 1) (ga/set-subpart :tgt e1 2)
        (ga/set-subpart :src e2 2) (ga/set-subpart :tgt e2 1)
        (ga/set-subpart :inv e1 e2) (ga/set-subpart :inv e2 e1))))

(deftest test-verified-migrate-checks-input-instance-axioms
  (when (ansatz-ready?)
    (testing "verified-migrate accepts an instance that satisfies its
              schema's axioms and rejects one that violates them"
      ;; Good instance: passes.
      (let [g (good-sym-graph)
            g' (bridge/verified-migrate IdSymAx g)]
        (is (= 2 (count (ga/parts g' :V)))))
      ;; Bad instance: a third edge whose :inv points wrong.
      (let [bad (let [g (good-sym-graph)
                      [g [e3]] (ga/add-parts g :E 1)]
                  (ga/set-subpart g :inv e3 1))]
        (is (thrown-with-msg? Exception #"ACSet axiom violation"
                              (bridge/verified-migrate IdSymAx bad)))))))

(deftest test-verified-migrate-check-instance-opt-out
  (when (ansatz-ready?)
    (testing ":check-instance? false skips the runtime check (useful when
              the instance is known-correct or the user wants the slower
              ansatz check alone)"
      (let [bad (let [g (good-sym-graph)
                      [g [e3]] (ga/add-parts g :E 1)]
                  (ga/set-subpart g :inv e3 1))
            g' (bridge/verified-migrate IdSymAx bad {:check-instance? false})]
        (is (some? g')
            "migration completes without checking instance axioms")))))
