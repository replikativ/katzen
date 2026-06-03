(ns katzen.diagram
  "Rendering wiring/string diagrams — a separate functor from the diagram to a
   layout (doc programs-as-diagrams.md §4: the diagram denotes a coherent
   morphism; what we draw is a *view* of it).

   Renders a diagram VALUE — the data of an attributed directed wiring diagram —
     {:name    str?
      :inputs  [{:port :label}]
      :boxes   [{:id :label :pure? :ins [port…] :out port}]
      :wires   [{:from port :to port}]
      :outputs [port]}
   to a target format. This shape is produced by `katzen.program` (Clojure code →
   diagram) and is the same shape a dynamical-system wiring diagram presents, so
   one renderer serves both. Backing it with the `katzen.dwd` ACSet (so rendering
   becomes the generic S→Comp render functor and the diagram composes via
   `oapply`) is the monoidal-layer phase; the renderer's input shape will not
   change.

   Currently emits **mermaid** (instant, embeddable). A ReactFlow/n8n-style
   interactive back end (typed handles ≈ ports) is the natural next target; it is
   the same data, a different serialization."
  (:require [clojure.string :as str]))

(defn- port->box
  "Every out-port → its box id, so wires (port→port) become box→box edges."
  [boxes]
  (into {} (for [{:keys [id out]} boxes :when out] [out id])))

(defn- node
  "A mermaid node line for a box, indented by `pad`. Shapes encode structure:
   stadium = input port, doubled border = pure (the cartesian bead), hexagon =
   a `cond` selection box, plain rectangle = an effecting/opaque box."
  [pad esc {:keys [id label pure? input? kind]}]
  (str pad id
       (cond
         input?            (str "([\"" (esc label) "\"])")        ; stadium = input port
         (= :cond kind)    (str "{{\"" (esc label) "\"}}")        ; hexagon = cond/selection
         (= :program kind) (str "[/\"" (esc label) "\"/]")        ; parallelogram = program code ⌜·⌝
         (= :run kind)     (str "[/\"" (esc label) "\"\\]")       ; trapezoid = apply (run)
         pure?             (str "[[\"" (esc label) "\"]]")        ; doubled border = pure
         :else             (str "[\"" (esc label) "\"]"))))

(defn- emit-groups
  "Recursively emit nodes for the boxes at `path` plus a `subgraph` for each
   branch group that extends `path` by one [cond-id guard] segment."
  [by-group pad esc path]
  (let [npath (count path)
        children (->> (keys by-group)
                      (filter #(and (> (count %) npath) (= path (subvec % 0 npath))))
                      (map #(subvec % 0 (inc npath)))
                      distinct)]
    (concat
     (map #(node pad esc %) (get by-group path))
     (mapcat (fn [cpath]
               (let [[cid guard] (last cpath)]
                 (concat [(str pad "subgraph sg_" cid "_" (clojure.core/name guard)
                               "[\"" (clojure.core/name guard) "\"]")]
                         (emit-groups by-group (str pad "  ") esc cpath)
                         [(str pad "end")])))
             children))))

(defn ->mermaid
  "Render a diagram value as a mermaid flowchart. Boxes are nodes; **pure boxes
   get a doubled border** (the cartesian bead `•` — what licenses CSE/DCE); a
   `:cond` box is a hexagon whose **branches are nested subgraphs** (the operadic
   fill; only one runs); wires are edges; a value used by several boxes shows as
   **fan-out** (copy `Δ`); an out-port with no edge is an implicit **delete** `▪`."
  [{:keys [name boxes wires outputs]}]
  (let [p->b (port->box boxes)
        esc  (fn [s] (-> (str s) (str/replace "\"" "'")))
        by-group (group-by #(:group % []) boxes)
        edge (fn [{:keys [from to]}]
               (let [s (p->b from) t (p->b to)]
                 (when (and s t (not= s t)) (str "  " s " --> " t))))]
    (str/join
     "\n"
     (concat
      [(str "%% " (or name "fn") " — string diagram  (⟦·⟧;  ‖ box ‖ = pure;  ⬡ = if)")
       "flowchart LR"]
      (emit-groups by-group "  " esc [])
      (->> wires (keep edge) distinct)
      (for [o outputs :let [b (p->b o)] :when b]
        (str "  " b " --> RESULT((result))"))))))
