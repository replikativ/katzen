(ns katzen.acset.graphs-test
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.migration :as m]
            [katzen.acset.graphs :as gg]))

;; ============================================================================
;; SchSymmetricGraph
;; ============================================================================

(deftest test-symmetric-graph-empty
  (let [g (gg/symmetric-graph)]
    (is (a/acset? g))
    (is (= 0 (a/nparts g :V)))
    (is (= 0 (a/nparts g :E)))
    (is (= gg/SchSymmetricGraph (a/schema g)))))

(deftest test-add-sym-edge
  (testing "add-sym-edge yields two edges with inv pointing at each other"
    (let [[g _]      (a/add-parts (gg/symmetric-graph) :V 2)
          [g e1 e2]  (gg/add-sym-edge g 1 2)]
      (is (= 2 (a/nparts g :E)))
      (is (= 1 (a/subpart g :src e1)))  (is (= 2 (a/subpart g :tgt e1)))
      (is (= 2 (a/subpart g :src e2)))  (is (= 1 (a/subpart g :tgt e2)))
      (is (= e2 (a/subpart g :inv e1)))
      (is (= e1 (a/subpart g :inv e2))))))

(deftest test-forget-symmetric
  (testing "Δ_ForgetSymmetric drops the inv hom; underlying digraph kept intact"
    (let [[g _]      (a/add-parts (gg/symmetric-graph) :V 3)
          [g _ _]    (gg/add-sym-edge g 1 2)
          [g _ _]    (gg/add-sym-edge g 2 3)
          g'         (m/migrate gg/ForgetSymmetric g)]
      (is (= a/SchGraph (a/schema g')))
      (is (= 3 (a/nv g')))
      (is (= 4 (a/ne g')) "both directions of each undirected edge are kept")
      (is (= #{[1 2] [2 1] [2 3] [3 2]}
             (set (mapv (fn [e] [(a/src g' e) (a/tgt g' e)]) (a/edges g'))))))))

;; ============================================================================
;; SchReflexiveGraph
;; ============================================================================

(deftest test-reflexive-graph-empty
  (let [g (gg/reflexive-graph)]
    (is (a/acset? g))
    (is (= 0 (a/nparts g :V)))
    (is (= 0 (a/nparts g :E)))
    (is (= gg/SchReflexiveGraph (a/schema g)))))

(deftest test-add-refl-vertex
  (testing "add-refl-vertex creates a vertex and its mandatory self-loop"
    (let [[g v e] (gg/add-refl-vertex (gg/reflexive-graph))]
      (is (= 1 (a/nparts g :V)))
      (is (= 1 (a/nparts g :E)))
      (is (= v (a/subpart g :src e)))
      (is (= v (a/subpart g :tgt e)))
      (is (= e (a/subpart g :refl v))))))

(deftest test-forget-reflexive
  (testing "Δ_ForgetReflexive keeps every edge including the loops; drops refl"
    (let [[g v1 _] (gg/add-refl-vertex (gg/reflexive-graph))
          [g v2 _] (gg/add-refl-vertex g)
          [g v3 _] (gg/add-refl-vertex g)
          [g _]    (gg/add-refl-edge g v1 v2)
          g'       (m/migrate gg/ForgetReflexive g)]
      (is (= a/SchGraph (a/schema g')))
      (is (= 3 (a/nv g')))
      (is (= 4 (a/ne g')) "3 self-loops + 1 non-loop edge")
      (is (= #{[1 1] [2 2] [3 3] [1 2]}
             (set (mapv (fn [e] [(a/src g' e) (a/tgt g' e)]) (a/edges g'))))))))

;; ============================================================================
;; SchWeightedGraph
;; ============================================================================

(deftest test-weighted-graph-empty
  (let [g (gg/weighted-graph)]
    (is (a/acset? g))
    (is (= 0 (a/nparts g :V)))
    (is (= 0 (a/nparts g :E)))
    (is (= gg/SchWeightedGraph (a/schema g)))))

(deftest test-add-weighted-edge
  (testing "add-weighted-edge stores src/tgt and weight"
    (let [[g _] (a/add-parts (gg/weighted-graph) :V 2)
          [g e] (gg/add-weighted-edge g 1 2 3.14)]
      (is (= 1   (a/subpart g :src e)))
      (is (= 2   (a/subpart g :tgt e)))
      (is (= 3.14 (a/subpart g :weight e))))))

(deftest test-forget-weight
  (testing "Δ_ForgetWeight yields the underlying digraph; weights gone"
    (let [[g _]  (a/add-parts (gg/weighted-graph) :V 3)
          [g _]  (gg/add-weighted-edge g 1 2 1.5)
          [g _]  (gg/add-weighted-edge g 2 3 2.7)
          [g _]  (gg/add-weighted-edge g 3 1 0.3)
          g'     (m/migrate gg/ForgetWeight g)]
      (is (= a/SchGraph (a/schema g')))
      (is (= 3 (a/nv g')))
      (is (= 3 (a/ne g')))
      (is (= [[1 2] [2 3] [3 1]]
             (mapv (fn [e] [(a/src g' e) (a/tgt g' e)]) (a/edges g')))))))

;; ============================================================================
;; Composition with OpGraph: forget then reverse
;; ============================================================================

(deftest test-compose-forget-then-op
  (testing "Migrate weighted graph through ForgetWeight, then OpGraph: reversed digraph"
    (let [[g _]  (a/add-parts (gg/weighted-graph) :V 3)
          [g _]  (gg/add-weighted-edge g 1 2 1.0)
          [g _]  (gg/add-weighted-edge g 2 3 2.0)
          plain  (m/migrate gg/ForgetWeight g)
          rev    (m/migrate m/OpGraph plain)]
      (is (= #{[2 1] [3 2]}
             (set (mapv (fn [e] [(a/src rev e) (a/tgt rev e)]) (a/edges rev))))))))
