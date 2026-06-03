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

(defn ->mermaid
  "Render a diagram value as a mermaid flowchart. Boxes are nodes; **pure boxes
   get a doubled border** (the cartesian bead `•` — what licenses CSE/DCE); wires
   are edges; a value used by several boxes shows as **fan-out** (copy `Δ`); an
   out-port with no edge is an implicit **delete** `▪`. Outputs flow to a final
   result node."
  [{:keys [name boxes wires outputs]}]
  (let [p->b (port->box boxes)
        esc  (fn [s] (-> (str s) (str/replace "\"" "'")))
        node (fn [{:keys [id label pure? input?]}]
               (cond
                 input? (str "  " id "([\"" (esc label) "\"])")   ; stadium = input port
                 pure?  (str "  " id "[[\"" (esc label) "\"]]")   ; doubled border = pure (bead)
                 :else  (str "  " id "[\"" (esc label) "\"]")))
        edge (fn [{:keys [from to]}]
               (let [s (p->b from) t (p->b to)]
                 (when (and s t (not= s t)) (str "  " s " --> " t))))]
    (str/join
     "\n"
     (concat
      [(str "%% " (or name "fn") " — string diagram  (⟦·⟧;  ‖ box ‖ = pure/cartesian)")
       "flowchart LR"]
      (map node boxes)
      (->> wires (keep edge) distinct)
      (for [o outputs :let [b (p->b o)] :when b]
        (str "  " b " --> RESULT((result))"))))))
