(ns katzen.acset.homomorphism-test
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.homomorphism :as hom]))

;; Helpers to build small graphs

(defn- path-graph
  "Path: 1 → 2 → 3 → ... → n. Returns the ACSet."
  [n]
  (let [[g vs] (a/add-vertices (a/graph) n)]
    (reduce (fn [g i] (first (a/add-edge g i (inc i))))
            g
            (range 1 n))))

(defn- cycle-graph
  "Cycle: 1 → 2 → ... → n → 1."
  [n]
  (let [g (path-graph n)
        [g _] (a/add-edge g n 1)]
    g))

(defn- triangle [] (cycle-graph 3))

(defn- complete-graph
  "Complete digraph on n vertices (n^2 edges, including self-loops)."
  [n]
  (let [[g _] (a/add-vertices (a/graph) n)]
    (reduce (fn [g [s t]] (first (a/add-edge g s t)))
            g
            (for [s (range 1 (inc n)) t (range 1 (inc n))] [s t]))))

;; ============================================================================
;; Trivial / edge cases
;; ============================================================================

(deftest test-empty-to-anything
  (testing "Empty source has exactly one homomorphism to anything"
    (is (= 1 (count (hom/homomorphisms (a/graph) (triangle)))))
    (is (= {} (hom/homomorphism (a/graph) (triangle))))))

(deftest test-identity-homomorphism
  (testing "Identity is a homomorphism from any graph to itself"
    (let [g (triangle)
          h (hom/homomorphism g g)]
      (is (some? h))
      ;; Identity map should be one possible answer (though not the only one).
      (let [per-ob (hom/assignment->per-object h)]
        (is (= 3 (count (get per-ob :V))))
        (is (= 3 (count (get per-ob :E))))))))

(deftest test-non-existence
  (testing "Path 1→2 has no homomorphism to a single isolated vertex"
    (let [src (path-graph 2)
          [tgt _] (a/add-vertex (a/graph))]
      (is (nil? (hom/homomorphism src tgt))))))

;; ============================================================================
;; Counting homomorphisms — basic graph-theoretic checks
;; ============================================================================

(deftest test-count-edges-in-target-is-hom-count-from-single-edge
  (testing "homs from single-edge graph to any graph = edges in target"
    (let [src (path-graph 2)]  ;; one edge 1→2
      (doseq [target [(triangle) (cycle-graph 4) (complete-graph 3)]]
        (is (= (a/ne target) (hom/nhomomorphisms src target))
            (str "expected " (a/ne target) " homs to graph with " (a/ne target) " edges"))))))

(deftest test-count-vertices-in-target-is-hom-count-from-single-vertex
  (testing "homs from single vertex to any graph = vertices in target"
    (let [[src _] (a/add-vertex (a/graph))]
      (doseq [target [(triangle) (cycle-graph 4) (complete-graph 3)]]
        (is (= (a/nv target) (hom/nhomomorphisms src target)))))))

;; ============================================================================
;; Loop / cycle structure
;; ============================================================================

(deftest test-loop-to-loop
  (testing "1-vertex 1-loop → 1-vertex 1-loop: exactly 1 hom"
    (let [loop-g (let [[g v] (a/add-vertex (a/graph))
                       [g _] (a/add-edge g v v)] g)]
      (is (= 1 (hom/nhomomorphisms loop-g loop-g))))))

(deftest test-loop-to-non-loop
  (testing "1-vertex 1-loop → triangle: 3 homs (each vertex's self-loop ... wait, triangle has no self-loops)"
    (let [loop-g (let [[g v] (a/add-vertex (a/graph))
                       [g _] (a/add-edge g v v)] g)
          tri    (triangle)]
      ;; Triangle 1→2→3→1 has no self-loops, so no hom from a loop graph.
      (is (= 0 (hom/nhomomorphisms loop-g tri))))))

(deftest test-loop-to-complete
  (testing "1-vertex 1-loop → K3 (with self-loops): hom count = vertex count"
    (let [loop-g (let [[g v] (a/add-vertex (a/graph))
                       [g _] (a/add-edge g v v)] g)]
      ;; Complete graph (n=3) includes all (s,t) pairs, including self-loops.
      ;; A loop at v ↦ a self-loop at some vertex of target. Target has 3 self-loops.
      (is (= 3 (hom/nhomomorphisms loop-g (complete-graph 3)))))))

;; ============================================================================
;; A larger sanity check
;; ============================================================================

(deftest test-path-to-cycle
  (testing "Path of length 3 → cycle of length 3"
    (let [src (path-graph 3)
          tgt (triangle)]
      ;; Path 1→2→3 can be embedded starting at each cycle vertex: 3 homs.
      (is (= 3 (hom/nhomomorphisms src tgt))))))
