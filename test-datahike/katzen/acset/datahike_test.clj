(ns katzen.acset.datahike-test
  "Verifies DatahikeACSet behaves the same as VectorACSet through the
   shared IACSet protocol, and that the backtracker works correctly
   against either backend."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.datahike :as dh]
            [katzen.acset.homomorphism :as hom]))

;; Helpers — same shape as the VectorACSet tests but parameterized over
;; a graph constructor.

(defn- build-path
  "Build a 3-vertex 2-edge path (1→2, 2→3) using the given graph ctor."
  [graph-ctor]
  (let [[g vs] (a/add-vertices (graph-ctor) 3)
        [g _]  (a/add-edge g (nth vs 0) (nth vs 1))
        [g _]  (a/add-edge g (nth vs 1) (nth vs 2))]
    [g vs]))

(defn- build-triangle [graph-ctor]
  (let [[g vs] (a/add-vertices (graph-ctor) 3)
        [g _]  (a/add-edge g (nth vs 0) (nth vs 1))
        [g _]  (a/add-edge g (nth vs 1) (nth vs 2))
        [g _]  (a/add-edge g (nth vs 2) (nth vs 0))]
    [g vs]))

(deftest test-datahike-basic-construction
  (testing "DatahikeACSet supports the same construction API as VectorACSet"
    (let [[g vs] (build-path dh/graph)]
      (is (a/acset? g))
      (is (= 3 (a/nv g)))
      (is (= 2 (a/ne g)))
      (is (= 3 (count vs)) "got 3 vertex ids back from add-vertices"))))

(deftest test-datahike-incident
  (testing "incident works as inverse-image lookup"
    (let [[g vs] (build-triangle dh/graph)
          [v1 v2 v3] vs]
      (is (= 1 (count (a/out-edges g v1))))
      (is (= 1 (count (a/out-edges g v2))))
      (is (= 1 (count (a/out-edges g v3))))
      (is (= 1 (count (a/in-edges g v1))))
      (is (= 1 (count (a/in-edges g v2))))
      (is (= 1 (count (a/in-edges g v3)))))))

(deftest test-datahike-subpart-and-src-tgt
  (testing "Edge endpoints survive the round trip"
    (let [[g vs] (build-path dh/graph)
          [v1 v2 v3] vs
          [e1 e2] (a/edges g)]
      (is (= v1 (a/src g e1)))
      (is (= v2 (a/tgt g e1)))
      (is (= v2 (a/src g e2)))
      (is (= v3 (a/tgt g e2))))))

;; ============================================================================
;; Cross-backend agreement — hom counts must match
;; ============================================================================

(deftest test-backends-agree-on-empty-to-anything
  (testing "Empty src → triangle: 1 hom (both backends)"
    (let [[tri-vec _] (build-triangle a/graph)
          [tri-dh _]  (build-triangle dh/graph)]
      (is (= 1 (count (hom/homomorphisms (a/graph) tri-vec))))
      (is (= 1 (count (hom/homomorphisms (dh/graph) tri-dh)))))))

(deftest test-backends-agree-single-edge-to-triangle
  (testing "Single edge → triangle: 3 homs (one per edge in target)"
    (let [[src-vec _] (build-path a/graph)  ;; reuse 3-vertex but only 1 edge worth
          single-edge-vec (let [[g _] (a/add-vertices (a/graph) 2)
                                [g _] (a/add-edge g 1 2)]
                            g)
          single-edge-dh  (let [[g vs] (a/add-vertices (dh/graph) 2)
                                [g _]  (a/add-edge g (first vs) (second vs))]
                            g)
          [tri-vec _] (build-triangle a/graph)
          [tri-dh _]  (build-triangle dh/graph)]
      (is (= 3 (hom/nhomomorphisms single-edge-vec tri-vec))
          "VectorACSet: single edge → triangle = 3")
      (is (= 3 (hom/nhomomorphisms single-edge-dh tri-dh))
          "DatahikeACSet: single edge → triangle = 3"))))

(deftest test-backends-agree-path-to-cycle
  (testing "Path 1→2→3 → triangle: 3 homs (one rotation per starting vertex)"
    (let [[src-vec _] (build-path a/graph)
          [src-dh _]  (build-path dh/graph)
          [tri-vec _] (build-triangle a/graph)
          [tri-dh _]  (build-triangle dh/graph)]
      (is (= 3 (hom/nhomomorphisms src-vec tri-vec)))
      (is (= 3 (hom/nhomomorphisms src-dh tri-dh))))))

(deftest test-backends-agree-triangle-self-homs
  (testing "Triangle → triangle: 3 endomorphisms (the rotations; no flips since digraph)"
    (let [[tri-vec _] (build-triangle a/graph)
          [tri-dh _]  (build-triangle dh/graph)]
      (is (= 3 (hom/nhomomorphisms tri-vec tri-vec)))
      (is (= 3 (hom/nhomomorphisms tri-dh tri-dh))))))

;; ============================================================================
;; Datalog engine matches backtracker on every case
;; ============================================================================

(require '[katzen.acset.homomorphism-datalog :as homd])

(deftest test-datalog-matches-backtracker-empty
  (testing "Empty src → triangle"
    (let [[tri _] (build-triangle dh/graph)]
      (is (= (hom/nhomomorphisms (dh/graph) tri)
             (homd/nhomomorphisms-datalog (dh/graph) tri))))))

(deftest test-datalog-matches-backtracker-single-edge
  (testing "Single edge → triangle"
    (let [src (let [[g vs] (a/add-vertices (dh/graph) 2)
                    [g _]  (a/add-edge g (first vs) (second vs))]
                g)
          [tri _] (build-triangle dh/graph)]
      (is (= (hom/nhomomorphisms src tri)
             (homd/nhomomorphisms-datalog src tri))))))

(deftest test-datalog-matches-backtracker-path-to-cycle
  (testing "Path 1→2→3 → triangle"
    (let [[src _] (build-path dh/graph)
          [tri _] (build-triangle dh/graph)]
      (is (= (hom/nhomomorphisms src tri)
             (homd/nhomomorphisms-datalog src tri))))))

(deftest test-datalog-matches-backtracker-triangle-self
  (testing "Triangle → triangle"
    (let [[tri _] (build-triangle dh/graph)]
      (is (= (hom/nhomomorphisms tri tri)
             (homd/nhomomorphisms-datalog tri tri))))))

;; Vector source + datahike target via the datalog engine
(deftest test-datalog-with-vector-source-datahike-target
  (testing "datalog hom search works with any IACSet source as long as target is datahike-backed"
    (let [src-vec (let [[g _] (a/add-vertices (a/graph) 2)
                        [g _] (a/add-edge g 1 2)]
                    g)
          [tri-dh _] (build-triangle dh/graph)]
      (is (= 3 (homd/nhomomorphisms-datalog src-vec tri-dh))))))

;; Attr morphisms (typed value columns) — the graph-only tests above miss
;; these; this exercises the attr-schema installation in the datahike backend.
(def SchLabeledGraph
  {:name 'SchLabeledGraph
   :objects [:V :E]
   :homs [{:name :src :dom :E :codom :V}
          {:name :tgt :dom :E :codom :V}]
   :attr-types [:String :Int]
   :attrs [{:name :label  :dom :V :codom :String}
           {:name :weight :dom :V :codom :Int}]})

(deftest test-datahike-attributes-roundtrip
  (testing "Attr morphisms install + round-trip on the datahike backend"
    (let [[g [v1 v2]] (a/add-parts (dh/datahike-acset SchLabeledGraph) :V 2)
          g (-> g
                (a/set-subpart :label v1 "alpha") (a/set-subpart :weight v1 7)
                (a/set-subpart :label v2 "beta")  (a/set-subpart :weight v2 9))]
      (is (= "alpha" (a/subpart g :label v1)) "string attr round-trips")
      (is (= 7 (a/subpart g :weight v1))       "int attr round-trips")
      (is (= #{"alpha" "beta"} (set (map second (a/subpart-all g :label)))))
      (is (= [v2] (a/incident g :label "beta")) "reverse lookup by attr value"))))
