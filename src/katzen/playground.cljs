(ns katzen.playground
  "Interactive string-diagram playground: paste a Clojure `defn`/`fn`, see its
   string diagram live (katzen.program → katzen.diagram/->reactflow → ELK layout
   → ReactFlow). The functor + emitter are the shared .cljc code; this ns is the
   browser shell (the rendering functor's interactive back end).

   Custom node draws Dusko's visual language: one input HANDLE per argument, a
   filled BEAD on the output port for pure/cartesian boxes, distinct shapes per
   kind, recursion/feedback as a dashed ↺ trace edge. Sub-flows (if-branches, fn
   bodies — the operadic nesting) are COLLAPSIBLE: click a group's label to fold
   its internals into one box and reroute its edges to the boundary."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.reader :as reader]
            [katzen.program :as prog]
            [katzen.diagram :as diagram]
            ["@xyflow/react" :refer [ReactFlow Background Controls MiniMap Handle Position
                                     applyNodeChanges applyEdgeChanges]]
            ["elkjs/lib/elk.bundled.js" :default ELK]))

(def elk (ELK.))
(declare layout! toggle-collapse!)

;; ---------------------------------------------------------------------------
;; Hierarchy helpers (collapse/expand of nested sub-flows)
;; ---------------------------------------------------------------------------

(defn- parent-map [nodes]
  (into {} (for [n nodes :when (:parentId n)] [(:id n) (:parentId n)])))

(defn- collapsed-anc
  "The OUTERMOST strict ancestor of `id` that is collapsed, or nil."
  [pmap collapsed id]
  (loop [p (get pmap id) top nil]
    (if (nil? p) top
        (recur (get pmap p) (if (contains? collapsed p) p top)))))

(defn- vis-id  [pmap collapsed id] (or (collapsed-anc pmap collapsed id) id))
(defn- hidden? [pmap collapsed id] (some? (collapsed-anc pmap collapsed id)))

(defn- collect-pos [acc node]
  (let [acc (assoc acc (.-id node) {:x (.-x node) :y (.-y node)
                                    :w (.-width node) :h (.-height node)})]
    (reduce collect-pos acc (or (.-children node) #js []))))

;; ---------------------------------------------------------------------------
;; Custom nodes — Dusko's marks (multiple input ports; bead on the output port)
;; ---------------------------------------------------------------------------

(defn- node-css [t pure?]
  (merge
   {:padding "6px 12px" :fontSize 12 :fontFamily "monospace" :minWidth 72
    :textAlign "center" :background "#fff" :border "1px solid #94a3b8" :borderRadius 6}
   (case t
     "input"   {:background "#f0fdf4" :border "1px solid #22c55e" :borderRadius 999}
     "result"  {:background "#fef2f2" :border "1px solid #ef4444" :borderRadius 999}
     "cond"    {:background "#fff7ed" :border "2px solid #f59e0b"}
     "program" {:background "#eef2ff" :border "1px solid #6366f1"}
     "run"     {:background "#ecfeff" :border "1px solid #06b6d4"}
     ("recur" "recursive") {:background "#faf5ff" :border "2px dashed #a855f7"}
     (if pure? {:border "2px solid #0f172a"} {}))))

(defn- in-handle-style [n i]
  {:top (str (js/Math.round (* (/ (inc i) (inc n)) 100)) "%")
   :background "#94a3b8" :width 7 :height 7 :border "none"})

(defn- out-handle-style [pure?]
  (if pure?
    {:background "#0f172a" :width 11 :height 11 :border "2px solid #fff"}
    {:background "#94a3b8" :width 7 :height 7 :border "none"}))

(defn- render-node [t ^js props]
  (let [d        (.-data props)
        label    (aget d "label")
        pure?    (aget d "pure?")
        dropped? (aget d "dropped?")
        n        (max 1 (or (aget d "inputs") 0))]
    (r/as-element
     [:div {:style (node-css t pure?)}
      (map (fn [i]
             [:> Handle {:key (str "in" i) :type "target" :position (.-Left Position)
                         :id (str "in-" i) :style (in-handle-style n i)}])
           (range n))
      [:span {:style {:whiteSpace "nowrap"}} label (when dropped? " ⏚")]
      [:> Handle {:type "source" :position (.-Right Position) :id "out"
                  :style (out-handle-style pure?)}]])))

(defn- render-group [^js props]
  ;; a sub-flow container (if-branch / fn body); the label toggles collapse.
  (let [id        (.-id props)
        collapsed? (boolean (.-collapsed (.-data props)))
        label     (aget (.-data props) "label")]
    (r/as-element
     [:div {:style {:width "100%" :height "100%" :position "relative" :pointerEvents "none"}}
      [:span {:on-click (fn [e] (.stopPropagation e) (toggle-collapse! id))
              :title "collapse / expand"
              :style {:position "absolute" :top 3 :left 8 :fontSize 10 :fontWeight 700
                      :letterSpacing "0.04em" :textTransform "uppercase" :color "#a78bfa"
                      :cursor "pointer" :pointerEvents "auto" :userSelect "none"}}
       (str (if collapsed? "▸ " "▾ ") label)]
      ;; default (id-less) handles so rerouted edges can attach when collapsed
      [:> Handle {:type "target" :position (.-Left Position) :style {:opacity 0}}]
      [:> Handle {:type "source" :position (.-Right Position) :style {:opacity 0}}]])))

(def ^:private node-types
  (let [mk (fn [t] (fn [props] (render-node t props)))]
    #js {"box" (mk "box") "cond" (mk "cond") "program" (mk "program")
         "run" (mk "run") "recur" (mk "recur") "recursive" (mk "recursive")
         "input" (mk "input") "result" (mk "result")
         "group" (fn [props] (render-group props))}))

;; ---------------------------------------------------------------------------
;; State + collapse-aware layout
;; ---------------------------------------------------------------------------

(defonce state (r/atom {:code "" :rf nil :collapsed #{} :nodes #js [] :edges #js [] :err nil}))

(defn- ->elk
  "Build an ELK graph from the VISIBLE nodes. Collapsed groups appear as leaves
   (no children); edges are remapped to each endpoint's visible ancestor."
  [vis-nodes pmap collapsed edges]
  (let [by-parent (group-by :parentId vis-nodes)
        node->elk (fn node->elk [n]
                    (let [kids (get by-parent (:id n))
                          collapsed? (contains? collapsed (:id n))]
                      (if (and (seq kids) (= "group" (:type n)))
                        {:id (:id n)
                         :children (mapv node->elk kids)
                         :layoutOptions {"elk.algorithm" "layered" "elk.direction" "RIGHT"
                                         "elk.padding" "[top=30,left=14,bottom=14,right=14]"}}
                        {:id (:id n) :width (if collapsed? 110 120) :height 40})))
        elk-edges (->> edges
                       (map (fn [e] [(vis-id pmap collapsed (:source e))
                                     (vis-id pmap collapsed (:target e))]))
                       (remove (fn [[s t]] (= s t)))
                       distinct
                       (map-indexed (fn [i [s t]] {:id (str "e" i) :sources [s] :targets [t]})))]
    #js {:id "root"
         :layoutOptions #js {"elk.algorithm" "layered" "elk.direction" "RIGHT"
                             "elk.hierarchyHandling" "INCLUDE_CHILDREN"
                             "elk.layered.spacing.nodeNodeBetweenLayers" "70"
                             "elk.spacing.nodeNode" "34"}
         :children (clj->js (mapv node->elk (get by-parent nil)))
         :edges (clj->js (vec elk-edges))}))

(defn- ->rf-nodes [vis-nodes collapsed pos]
  (clj->js
   (for [{:keys [id type parentId data]} vis-nodes
         :let [{:keys [x y w h]} (get pos id {:x 0 :y 0})
               collapsed? (contains? collapsed id)]]
     (cond-> {:id id :type type
              :data (assoc (or data {:label id}) :collapsed collapsed?)
              :position {:x x :y y}}
       (= "group" type) (assoc :style {:width w :height h
                                       :background (if collapsed? "#f5f3ff" "rgba(148,163,184,0.06)")
                                       :border "1px dashed #94a3b8" :borderRadius 8})
       parentId (assoc :parentId parentId :extent "parent")))))

(defn- ->rf-edges [edges pmap collapsed]
  (let [seen (atom #{})]
    (clj->js
     (for [{:keys [source target sourceHandle targetHandle trace?]} edges
           :let [s (vis-id pmap collapsed source) t (vis-id pmap collapsed target)
                 eid (str s "->" t (when trace? "~"))]
           :when (and (not= s t) (not (@seen eid)))]
       (do (swap! seen conj eid)
           (cond-> {:id eid :source s :target t}
             (= s source) (assoc :sourceHandle sourceHandle)   ; keep handle only if not rerouted
             (= t target) (assoc :targetHandle targetHandle)
             trace? (assoc :animated true :label "↺"
                           :style {:stroke "#a855f7" :strokeWidth 1.5 :strokeDasharray "6 4"})))))))

(defn layout! []
  (let [{:keys [rf collapsed]} @state]
    (when rf
      (let [nodes (:nodes rf) edges (:edges rf)
            pmap  (parent-map nodes)
            vis   (remove #(hidden? pmap collapsed (:id %)) nodes)]
        (-> (.layout elk (->elk vis pmap collapsed edges))
            (.then (fn [res]
                     (swap! state assoc
                            :nodes (->rf-nodes vis collapsed (reduce collect-pos {} (.-children res)))
                            :edges (->rf-edges edges pmap collapsed)
                            :err nil)))
            (.catch (fn [e] (swap! state assoc :err (str "layout: " e)))))))))

(defn toggle-collapse! [id]
  (swap! state update :collapsed #(if (contains? % id) (disj % id) (conj % id)))
  (layout!))

;; ---------------------------------------------------------------------------
;; Examples + recompute
;; ---------------------------------------------------------------------------

(def control-example
  {:name "uav-control"
   :inputs [{:port "e" :label "e"} {:port "d" :label "d"}]
   :boxes [{:id "in_e" :label "e (setpoint)" :input? true :ins [] :out "e" :group []}
           {:id "in_d" :label "d (setpoint)" :input? true :ins [] :out "d" :group []}
           {:id "sensor"     :label "sensor"     :ins ["theta" "e"] :out "sp" :group []}
           {:id "controller" :label "controller" :ins ["sp" "d"]    :out "c"  :group []}
           {:id "plant"      :label "plant"      :ins ["c"]         :out "theta" :group []}]
   :wires [{:from "theta" :to "sp" :trace? true}
           {:from "e" :to "sp"}
           {:from "sp" :to "c"} {:from "d" :to "c"}
           {:from "c" :to "theta"}]
   :outputs ["theta"]})

(def examples
  [{:label "factorial — recursion (trace)"
    :code "(defn fact [n acc]\n  (if (pos? n)\n    (recur (dec n) (* n acc))\n    acc))"}
   {:label "stats — let, copy, dead binding"
    :code "(defn stats [xs]\n  (let [n      (count xs)\n        total  (reduce + 0 xs)\n        mean   (/ total n)\n        unused (first xs)]\n    (vector mean n)))"}
   {:label "scale-all — higher-order (map + fn)"
    :code "(defn scale-all [xs k]\n  (map (fn [x] (* x k)) xs))"}
   {:label "classify — nested conditionals"
    :code "(defn classify [x]\n  (if (pos? x) :pos\n    (if (neg? x) :neg :zero)))"}
   {:label "UAV control loop — directed machines (dynamics)"
    :diagram control-example
    :code ";; A CONTROL SYSTEM, not Clojure code — the SAME diagram substrate.\n;; sensor → controller → plant, with the plant output fed back to the\n;; sensor (the ↺ trace). Outer inputs: setpoints e, d.  (See doc/composition.md.)"}])

(def default-code (:code (first examples)))

(defn- def-form? [form]
  (and (seq? form) (contains? '#{defn defn- fn fn*} (first form))))

(defn render-diagram! [dia]
  (swap! state assoc :rf (diagram/->reactflow dia) :collapsed #{})
  (layout!))

(defn recompute! [code]
  (try
    (let [form (reader/read-string code)]
      (if-not (def-form? form)
        (swap! state assoc :err "Paste a (defn …) or (fn …) form.")
        (render-diagram! (prog/fn->diagram form))))
    (catch :default e (swap! state assoc :err (str "read: " (.-message e))))))

(defn load-example! [{:keys [code diagram]}]
  (swap! state assoc :code code :err nil)
  (if diagram (render-diagram! diagram) (recompute! code)))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(defn app []
  (let [{:keys [code nodes edges err]} @state]
    [:div {:style {:display "flex" :height "100vh" :fontFamily "monospace"}}
     [:div {:style {:width "36%" :display "flex" :flexDirection "column" :borderRight "1px solid #e1e4e8"}}
      [:div {:style {:padding "8px 12px" :background "#f6f8fa" :borderBottom "1px solid #e1e4e8"
                     :fontWeight 600 :display "flex" :justifyContent "space-between" :alignItems "center" :gap 8}}
       [:span "katzen — Clojure as a string diagram"]
       [:select {:style {:fontSize 11 :fontFamily "inherit" :padding "2px 4px"}
                 :on-change #(load-example! (nth examples (js/parseInt (.. % -target -value))))}
        (map-indexed (fn [i {:keys [label]}] [:option {:key i :value i} label]) examples)]]
      [:textarea {:value code :spellCheck false
                  :on-change #(let [v (.. % -target -value)]
                                (swap! state assoc :code v) (recompute! v))
                  :style {:flex 1 :border "none" :outline "none" :resize "none"
                          :padding "12px" :fontSize 13 :fontFamily "monospace" :tabSize 2}}]
      [:div {:style {:padding "6px 12px" :fontSize 11 :color "#586069" :borderTop "1px solid #e1e4e8" :lineHeight 1.6}}
       "● on output = pure (cartesian) · multiple ports = multiple args · ⏚ = dropped binding"
       [:br] "nested boxes = if / fn body (click ▾ to collapse) · ↺ dashed = recursion / feedback"
       [:br] "diagrams follow "
       [:a {:href "https://link.springer.com/book/10.1007/978-3-031-34827-3"
            :target "_blank" :rel "noopener" :style {:color "#6366f1"}}
        "Pavlović, Programs as Diagrams (Springer 2023)"]]
      (when err [:pre {:style {:color "#b00" :padding "8px 12px" :margin 0 :fontSize 12}} err])]
     [:div {:style {:flex 1}}
      [:> ReactFlow {:nodes nodes :edges edges :nodeTypes node-types :fitView true :minZoom 0.15
                     :onNodesChange (fn [changes]
                                      (swap! state update :nodes #(applyNodeChanges changes %)))
                     :onEdgesChange (fn [changes]
                                      (swap! state update :edges #(applyEdgeChanges changes %)))}
       [:> Background]
       [:> Controls]
       [:> MiniMap {:pannable true :zoomable true}]]]]))

(defn init! []
  (recompute! default-code)
  (rdom/render [app] (js/document.getElementById "app")))
