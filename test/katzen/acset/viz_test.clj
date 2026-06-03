(ns katzen.acset.viz-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [katzen.acset :as a]
            [katzen.acset.graphs :as gg]
            [katzen.acset.viz :as viz]))

;; ============================================================================
;; schema->dot
;; ============================================================================

(deftest test-schema-dot-shape
  (let [out (viz/schema->dot a/SchGraph)]
    (is (str/starts-with? out "digraph"))
    (is (str/includes? out "SchGraph"))
    (is (str/includes? out "\"V\" [shape=\"circle\"]"))
    (is (str/includes? out "\"E\" [shape=\"circle\"]"))
    (is (str/includes? out "\"E\" -> \"V\" [label=\"src\"]"))
    (is (str/includes? out "\"E\" -> \"V\" [label=\"tgt\"]"))
    (is (str/ends-with? (str/trimr out) "}"))))

(deftest test-schema-dot-symmetric
  (let [out (viz/schema->dot gg/SchSymmetricGraph)]
    (is (str/includes? out "\"E\" -> \"E\" [label=\"inv\"]")
        "inv: E → E is a self-loop on E")))

(deftest test-schema-dot-attrs
  (testing "Attr-types are boxed; attrs are dashed edges to them"
    (let [out (viz/schema->dot gg/SchWeightedGraph)]
      (is (str/includes? out "\"Weight\" [shape=\"box\", style=\"rounded\"]"))
      (is (str/includes? out "\"E\" -> \"Weight\"")
          "weight attr edge present")
      (is (str/includes? out "style=\"dashed\"")
          "attr edges are dashed"))))

(deftest test-schema-dot-name-override
  (let [out (viz/schema->dot a/SchGraph {:name "MyGraph"})]
    (is (str/includes? out "digraph \"MyGraph\""))))

;; ============================================================================
;; graph->dot
;; ============================================================================

(deftest test-graph-dot-triangle
  (let [[g _] (a/add-vertices (a/graph) 3)
        [g _] (a/add-edge g 1 2)
        [g _] (a/add-edge g 2 3)
        [g _] (a/add-edge g 3 1)
        out   (viz/graph->dot g)]
    (is (str/includes? out "\"1\" -> \"2\""))
    (is (str/includes? out "\"2\" -> \"3\""))
    (is (str/includes? out "\"3\" -> \"1\""))
    (testing "edge labels default to the part-id"
      (is (str/includes? out "label=\"1\"") "edge 1's label"))))

(deftest test-graph-dot-vertex-label
  (let [[g _] (a/add-vertices (a/graph) 2)
        [g _] (a/add-edge g 1 2)
        out   (viz/graph->dot g {:vertex-label (fn [v] (str "node-" v))})]
    (is (str/includes? out "label=\"node-1\""))
    (is (str/includes? out "label=\"node-2\""))))

(deftest test-graph-dot-weight-label
  (let [[g _] (a/add-parts (gg/weighted-graph) :V 2)
        [g _] (gg/add-weighted-edge g 1 2 3.14)
        out   (viz/graph->dot g {:edge-label viz/weight-edge-label})]
    (is (str/includes? out "[w=3.14]"))))

(deftest test-graph-dot-skips-unfinished-edges
  (testing "Edges missing src or tgt are silently dropped"
    (let [[g _] (a/add-vertices (a/graph) 1)
          [g e] (a/add-part g :E)
          g     (a/set-subpart g :src e 1)        ; tgt unset
          out   (viz/graph->dot g)]
      (is (not (str/includes? out "->"))
          "no arrow rendered for the partially-set edge"))))

(deftest test-graph-dot-rejects-non-graph-schema
  (testing "Schema without :V :E :src :tgt throws a clear error"
    ;; Build an ACSet on a schema that doesn't match the digraph shape.
    (let [bad-schema {:name 'Bad :objects [:X] :homs [] :attr-types [] :attrs []}
          a         (a/vector-acset bad-schema)]
      (is (thrown-with-msg? Exception #"graph->dot expects a schema with"
                            (viz/graph->dot a))))))
