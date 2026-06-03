(ns katzen.acset.graphs
  "Canonical graph schemas beyond plain digraphs, plus schema morphisms
   relating them.

   Mirrors Catlab.jl's `Graphs.BasicGraphs`:

   - SchSymmetricGraph — adds inv : E → E (an involution)
   - SchReflexiveGraph — adds refl : V → E (every vertex has a self-loop)
   - SchWeightedGraph  — adds an attr weight : E → Weight

   The inclusion morphisms SchGraph → Sch{Symmetric,Reflexive,Weighted}Graph
   induce Δ-migrations that *forget* the extra structure: a symmetric graph
   becomes its underlying digraph (both edges of each pair kept), a reflexive
   graph keeps every edge including its loops, and a weighted graph drops
   the weight column.

   Axioms (inv ∘ inv = id_E, refl ∘ src = id_V, etc.) are documented but
   not enforced at the schema-map level. Enforcement happens via the
   theory layer through ansatz; here we record the data shape only."
  (:require [katzen.acset :as a]
            [katzen.acset.migration :as m]))

;; ============================================================================
;; SchSymmetricGraph
;; ============================================================================

(def SchSymmetricGraph
  "Schema of symmetric (undirected, multi-edged) graphs. Each edge e has
   an involutive partner inv(e) with src(inv e) = tgt(e), tgt(inv e) = src(e),
   and inv(inv e) = e."
  {:name       'SchSymmetricGraph
   :objects    [:V :E]
   :homs       [{:name :src :dom :E :codom :V}
                {:name :tgt :dom :E :codom :V}
                {:name :inv :dom :E :codom :E}]
   :attr-types []
   :attrs      []})

(defn symmetric-graph
  "Empty symmetric graph."
  []
  (a/vector-acset SchSymmetricGraph))

(defn add-sym-edge
  "Add a symmetric edge between u and v: allocates two edges e1, e2 with
   src(e1)=u, tgt(e1)=v, src(e2)=v, tgt(e2)=u, inv(e1)=e2, inv(e2)=e1.
   Returns [new-graph e1 e2]."
  [g u v]
  (let [[g [e1 e2]] (a/add-parts g :E 2)
        g (-> g
              (a/set-subpart :src e1 u) (a/set-subpart :tgt e1 v)
              (a/set-subpart :src e2 v) (a/set-subpart :tgt e2 u)
              (a/set-subpart :inv e1 e2) (a/set-subpart :inv e2 e1))]
    [g e1 e2]))

;; ============================================================================
;; SchReflexiveGraph
;; ============================================================================

(def SchReflexiveGraph
  "Schema of reflexive graphs. Each vertex v has a distinguished self-loop
   refl(v) with src(refl v) = v, tgt(refl v) = v."
  {:name       'SchReflexiveGraph
   :objects    [:V :E]
   :homs       [{:name :src  :dom :E :codom :V}
                {:name :tgt  :dom :E :codom :V}
                {:name :refl :dom :V :codom :E}]
   :attr-types []
   :attrs      []})

(defn reflexive-graph
  "Empty reflexive graph."
  []
  (a/vector-acset SchReflexiveGraph))

(defn add-refl-vertex
  "Add a vertex and its mandatory self-loop. Returns [new-graph v e-refl]."
  [g]
  (let [[g v] (a/add-part g :V)
        [g e] (a/add-part g :E)
        g (-> g
              (a/set-subpart :src e v)
              (a/set-subpart :tgt e v)
              (a/set-subpart :refl v e))]
    [g v e]))

(defn add-refl-edge
  "Add a (non-reflexive) edge between two existing vertices. Returns
   [new-graph e]. The reflexive loops on u, v are unaffected."
  [g u v]
  (let [[g e] (a/add-part g :E)]
    [(-> g (a/set-subpart :src e u) (a/set-subpart :tgt e v)) e]))

;; ============================================================================
;; SchWeightedGraph
;; ============================================================================

(def SchWeightedGraph
  "Schema of edge-weighted directed graphs: every edge carries a Weight."
  {:name       'SchWeightedGraph
   :objects    [:V :E]
   :homs       [{:name :src :dom :E :codom :V}
                {:name :tgt :dom :E :codom :V}]
   :attr-types [:Weight]
   :attrs      [{:name :weight :dom :E :codom :Weight}]})

(defn weighted-graph
  "Empty weighted graph."
  []
  (a/vector-acset SchWeightedGraph))

(defn add-weighted-edge
  "Add an edge from u to v with weight w. Returns [new-graph e]."
  [g u v w]
  (let [[g e] (a/add-part g :E)]
    [(-> g
         (a/set-subpart :src e u)
         (a/set-subpart :tgt e v)
         (a/set-subpart :weight e w))
     e]))

;; ============================================================================
;; Forget morphisms — schema inclusions SchGraph → Sch{...}Graph
;; ============================================================================
;;
;; Each is a schema *inclusion*: the smaller schema (SchGraph) embeds into a
;; richer one. The induced Δ-migration takes data on the richer schema and
;; *forgets* the extra structure. The result lives on plain SchGraph.

(def ForgetSymmetric
  "Inclusion SchGraph ↪ SchSymmetricGraph. Δ takes a symmetric graph to its
   underlying digraph, keeping every edge (each pair of inverse partners
   appears as two separate edges)."
  (m/schema-morphism 'ForgetSymmetric a/SchGraph SchSymmetricGraph
                     {:V :V, :E :E}
                     {:src [:src], :tgt [:tgt]}))

(def ForgetReflexive
  "Inclusion SchGraph ↪ SchReflexiveGraph. Δ takes a reflexive graph to its
   underlying digraph; reflexive loops survive as ordinary self-edges."
  (m/schema-morphism 'ForgetReflexive a/SchGraph SchReflexiveGraph
                     {:V :V, :E :E}
                     {:src [:src], :tgt [:tgt]}))

(def ForgetWeight
  "Inclusion SchGraph ↪ SchWeightedGraph. Δ drops the weight attribute."
  (m/schema-morphism 'ForgetWeight a/SchGraph SchWeightedGraph
                     {:V :V, :E :E}
                     {:src [:src], :tgt [:tgt]}))
