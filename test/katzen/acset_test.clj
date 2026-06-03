(ns katzen.acset-test
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]))

(deftest test-empty-graph
  (testing "Empty graph"
    (let [g (a/graph)]
      (is (a/acset? g))
      (is (= 0 (a/nv g)))
      (is (= 0 (a/ne g)))
      (is (= '() (a/vertices g)))
      (is (= '() (a/edges g)))
      (is (= a/SchGraph (a/schema g))))))

(deftest test-add-vertices-and-edges
  (testing "Build a triangle graph: 3 vertices, 3 edges 1→2, 2→3, 3→1"
    (let [[g vs] (a/add-vertices (a/graph) 3)
          _ (is (= [1 2 3] vs))
          [g e1] (a/add-edge g 1 2)
          [g e2] (a/add-edge g 2 3)
          [g e3] (a/add-edge g 3 1)]
      (is (= [1 2 3] [e1 e2 e3]))
      (is (= 3 (a/nv g)))
      (is (= 3 (a/ne g)))
      (is (= 1 (a/src g 1)))
      (is (= 2 (a/tgt g 1)))
      (is (= 2 (a/src g 2)))
      (is (= 3 (a/tgt g 2)))
      (is (= 3 (a/src g 3)))
      (is (= 1 (a/tgt g 3))))))

(deftest test-incident
  (testing "incident is the inverse-image lookup"
    (let [[g _] (a/add-vertices (a/graph) 3)
          [g _] (a/add-edge g 1 2)
          [g _] (a/add-edge g 1 3)
          [g _] (a/add-edge g 2 3)]
      (is (= [1 2] (a/out-edges g 1)) "edges from vertex 1")
      (is (= [3]   (a/out-edges g 2)) "edges from vertex 2")
      (is (= []    (a/out-edges g 3)) "no edges from vertex 3")
      (is (= []    (a/in-edges g 1))  "no edges to vertex 1")
      (is (= [1]   (a/in-edges g 2))  "edge to vertex 2")
      (is (= [2 3] (a/in-edges g 3))  "edges to vertex 3"))))

(deftest test-add-part-with
  (testing "add-part-with for ergonomic edge construction"
    (let [g (-> (a/graph)
                (a/add-part-with :V {})              ;; v1
                (a/add-part-with :V {})              ;; v2
                (a/add-part-with :V {})              ;; v3
                (a/add-part-with :E {:src 1 :tgt 2})  ;; e1
                (a/add-part-with :E {:src 2 :tgt 3})  ;; e2
                (a/add-part-with :E {:src 1 :tgt 3})) ;; e3
          ]
      (is (= 3 (a/nv g)))
      (is (= 3 (a/ne g)))
      (is (= [1 1] [(a/src g 1) (a/src g 3)]))
      (is (= [2 3] [(a/tgt g 1) (a/tgt g 2)])))))

(deftest test-add-parts-with
  (testing "add-parts-with allocates multiple parts each with their own
            subpart values, returning the new acset and the new ids"
    (let [[g _]   (a/add-vertices (a/graph) 4)
          [g ids] (a/add-parts-with g :E
                                    [{:src 1 :tgt 2}
                                     {:src 2 :tgt 3}
                                     {:src 3 :tgt 4}])]
      (is (= 4 (a/nv g)))
      (is (= 3 (a/ne g)))
      (is (= 3 (count ids)))
      (is (= [1 2 3] (mapv #(a/src g %) ids)))
      (is (= [2 3 4] (mapv #(a/tgt g %) ids))))))

(deftest test-rem-part
  (testing "rem-part removes the part and its outbound morphism values"
    (let [[g _] (a/add-vertices (a/graph) 3)
          [g _] (a/add-edge g 1 2)
          [g _] (a/add-edge g 2 3)
          g'    (a/rem-part g :E 1)]
      (is (= 1 (a/ne g')) "edge count drops by 1")
      (is (= [2] (a/edges g')))
      (is (nil? (a/src g' 1)) "removed edge's src is gone")
      ;; Vertex removal — note: does NOT cascade to incoming edges.
      ;; Caller's responsibility (matches Catlab's rem_part! semantics).
      (let [g'' (a/rem-part g' :V 3)]
        (is (= 2 (a/nv g'')) "vertex count drops")
        (is (= [1 2] (a/vertices g'')))))))

(deftest test-persistence
  (testing "Operations are non-mutating — original graph is unchanged"
    (let [g0 (a/graph)
          [g1 _] (a/add-vertex g0)
          [g2 _] (a/add-vertex g1)]
      (is (= 0 (a/nv g0)) "original empty graph still empty")
      (is (= 1 (a/nv g1)) "g1 has 1 vertex")
      (is (= 2 (a/nv g2)) "g2 has 2"))))
