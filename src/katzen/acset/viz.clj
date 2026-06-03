(ns katzen.acset.viz
  "Graphviz dot emitters for ACSet schemas and graph-like ACSet instances.

   These functions produce dot source strings; rendering is the caller's
   responsibility (typically `dot -Tpng < out.dot > out.png`, or a notebook
   helper). No shell or process invocation happens here.

   Schemas are rendered as their underlying typed-quiver: objects are
   circular nodes, attr-types are boxed nodes, homs are solid directed
   edges, attrs are dashed directed edges.

   Graph-like ACSets (any schema with objects :V, :E and homs :src, :tgt
   from E to V) are rendered as the obvious labeled digraph. The vertex-
   and edge-label functions let callers attach attr values, weights, or
   any other annotation to the rendered nodes/edges."
  (:require [clojure.string :as str]
            [katzen.acset :as a]))

;; ============================================================================
;; Shared helpers
;; ============================================================================

(defn- as-text
  "Best-effort string rendering for dot tokens: keywords lose their colon,
   symbols/strings/numbers stringify normally."
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    :else        (str x)))

(defn- safe-id
  "Escape a name so it's a valid dot identifier without quoting acrobatics.
   We just quote everything — dot accepts \"...\" anywhere an ID is wanted."
  [x]
  (str \" (str/escape (as-text x) {\" "\\\"", \\ "\\\\"}) \"))

(defn- attr-list
  "Render a {:key value} map as a dot attribute list. Skips nil values.
   Returns the empty string if nothing remains."
  [m]
  (let [pairs (->> m
                   (keep (fn [[k v]] (when (some? v) [k v])))
                   (map (fn [[k v]] (str (name k) "=" (safe-id v)))))]
    (if (seq pairs)
      (str " [" (str/join ", " pairs) "]")
      "")))

;; ============================================================================
;; Schemas → dot
;; ============================================================================

(defn schema->dot
  "Render an ACSet schema as Graphviz dot source.

   Options:
     :name       Graph name override (default = schema's :name)
     :rankdir    \"LR\" (default) or \"TB\""
  ([schema] (schema->dot schema {}))
  ([schema {:keys [name rankdir] :or {rankdir "LR"}}]
   (let [gname (or name (:name schema) 'Schema)
         lines
         (concat
          [(str "digraph " (safe-id gname) " {")
           (str "  rankdir=" rankdir ";")
           "  node [fontname=\"Helvetica\"];"
           "  edge [fontname=\"Helvetica\"];"
           ""
           "  // Objects"]
          (for [ob (:objects schema)]
            (str "  " (safe-id ob) (attr-list {:shape "circle"})))
          (when (seq (:attr-types schema))
            (cons "" (cons "  // Attr-types"
                           (for [t (:attr-types schema)]
                             (str "  " (safe-id t)
                                  (attr-list {:shape "box" :style "rounded"}))))))
          (when (seq (:homs schema))
            (cons "" (cons "  // Homs"
                           (for [{:keys [name dom codom]} (:homs schema)]
                             (str "  " (safe-id dom) " -> " (safe-id codom)
                                  (attr-list {:label (clojure.core/name name)}))))))
          (when (seq (:attrs schema))
            (cons "" (cons "  // Attrs"
                           (for [{:keys [name dom codom]} (:attrs schema)]
                             (str "  " (safe-id dom) " -> " (safe-id codom)
                                  (attr-list {:label (clojure.core/name name)
                                              :style "dashed"}))))))
          ["}"])]
     (str/join "\n" lines))))

;; ============================================================================
;; Graph-like ACSets → dot
;; ============================================================================

(defn- has-graph-shape?
  "Schema looks like a digraph: contains :V, :E and homs :src, :tgt: E → V."
  [schema]
  (let [obs (set (:objects schema))]
    (and (obs :V) (obs :E)
         (some #(and (= :src (:name %)) (= :E (:dom %)) (= :V (:codom %))) (:homs schema))
         (some #(and (= :tgt (:name %)) (= :E (:dom %)) (= :V (:codom %))) (:homs schema)))))

(defn graph->dot
  "Render a graph-like ACSet (any schema with :V, :E and :src, :tgt) as
   Graphviz dot source.

   Options:
     :name          Graph name (default \"G\")
     :rankdir       \"LR\" (default) or \"TB\"
     :vertex-label  fn vertex-id → string; default str
     :edge-label    fn [acset edge-id] → string; default str of edge-id"
  ([acset] (graph->dot acset {}))
  ([acset {:keys [name rankdir vertex-label edge-label]
           :or {name "G", rankdir "LR"
                vertex-label str
                edge-label (fn [_ e] (str e))}}]
   (let [sch (a/schema acset)]
     (when-not (has-graph-shape? sch)
       (throw (ex-info "graph->dot expects a schema with :V, :E and :src, :tgt"
                       {:schema (clojure.core/name (:name sch))}))))
   (let [lines
         (concat
          [(str "digraph " (safe-id name) " {")
           (str "  rankdir=" rankdir ";")
           "  node [shape=circle, fontname=\"Helvetica\"];"
           "  edge [fontname=\"Helvetica\"];"
           ""]
          (for [v (a/parts acset :V)]
            (str "  " (safe-id v) (attr-list {:label (vertex-label v)})))
          [""]
          (for [e (a/parts acset :E)
                :let [s (a/subpart acset :src e)
                      t (a/subpart acset :tgt e)]
                :when (and s t)]
            (str "  " (safe-id s) " -> " (safe-id t)
                 (attr-list {:label (edge-label acset e)})))
          ["}"])]
     (str/join "\n" lines))))

;; ============================================================================
;; Convenience: a weight-aware edge label for weighted graphs
;; ============================================================================

(defn weight-edge-label
  "Edge-label function for graph->dot on weighted graphs: renders
   \"e<id> [w=<weight>]\"."
  [acset e]
  (let [w (a/subpart acset :weight e)]
    (if (some? w)
      (str "e" e " [w=" w "]")
      (str "e" e))))
