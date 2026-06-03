(ns katzen.acset.colimits-test
  "Colimits of ACSets: coproduct (disjoint union) and pushout (gluing).
   Validated on graphs — the canonical example — plus attribute carry."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.colimits :as col]
            [katzen.acset.morphism :as am]))

(defn- graph
  "A SchGraph ACSet: `nv` vertices and `edges` = [[src tgt] …] (part ids)."
  [edges nv]
  (let [g0 (first (a/add-parts (a/vector-acset a/SchGraph) :V nv))
        [g eids] (a/add-parts g0 :E (count edges))]
    (reduce (fn [g [e [s t]]] (-> g (a/set-subpart :src e s) (a/set-subpart :tgt e t)))
            g (map vector eids edges))))

(defn- edges-of [acset]
  (set (for [e (a/parts acset :E)] [(a/subpart acset :src e) (a/subpart acset :tgt e)])))

(deftest coproduct-of-graphs
  (let [X (graph [[1 2]] 2)
        Y (graph [[1 2]] 2)
        {:keys [apex legs]} (col/coproduct X Y)]
    (is (= 4 (a/nparts apex :V)) "disjoint union of vertices")
    (is (= 2 (a/nparts apex :E)) "disjoint union of edges")
    (is (= 2 (count (edges-of apex))) "two distinct edges, no vertices shared")
    (is (am/natural? (first legs)))
    (is (am/natural? (second legs)) "both injections are natural")))

(deftest pushout-glues-graphs-at-a-vertex
  (testing "X(•→•) ⊔_A Y(•→•) identifying X.tgt with Y.src ⇒ a length-2 path"
    (let [X  (graph [[1 2]] 2)
          Y  (graph [[1 2]] 2)
          A  (first (a/add-parts (a/vector-acset a/SchGraph) :V 1))
          av (first (a/parts A :V))
          f  (am/->ACSetMorphism A X {:V {av 2} :E {}})   ; apex vertex → X target
          g  (am/->ACSetMorphism A Y {:V {av 1} :E {}})   ; apex vertex → Y source
          {:keys [apex legs]} (col/pushout f g)]
      (is (= 3 (a/nparts apex :V)) "4 vertices minus the 1 identification")
      (is (= 2 (a/nparts apex :E)) "edges are not glued")
      (is (am/natural? (first legs)))
      (is (am/natural? (second legs)))
      (is (= (get-in (first legs)  [:components :V 2])
             (get-in (second legs) [:components :V 1]))
          "the glued vertex is a single part in the pushout")
      (let [path (edges-of apex)
            [[s1 t1] [s2 t2]] (vec path)]
        (is (or (= t1 s2) (= t2 s1)) "the two edges chain through the shared vertex")))))

(deftest coproduct-carries-attributes
  (testing "values on attributes are carried into the disjoint union"
    (let [sch {:name 'Labeled :objects [:N] :homs []
               :attr-types [:Name] :attrs [{:name :label :dom :N :codom :Name}]}
          mk  (fn [lbl] (let [[g [n]] (a/add-parts (a/vector-acset sch) :N 1)]
                          (a/set-subpart g :label n lbl)))
          X   (mk "a")
          Y   (mk "b")
          {:keys [apex]} (col/coproduct X Y)]
      (is (= 2 (a/nparts apex :N)))
      (is (= #{"a" "b"} (set (for [n (a/parts apex :N)] (a/subpart apex :label n))))
          "both labels survive the coproduct"))))
