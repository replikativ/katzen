(ns katzen.playground
  "Interactive string-diagram playground: paste a Clojure `defn`/`fn`, see its
   string diagram live (katzen.program → katzen.diagram/->reactflow → ELK layout
   → ReactFlow). The functor + emitter are the shared .cljc code; this ns is the
   browser shell (the rendering functor's interactive back end).

   Custom node draws Dusko's visual language: one input HANDLE per argument (his
   multiple input strings), a filled BEAD on the output port for pure/cartesian
   boxes, distinct shapes per kind, and recursion as a dashed `↺` trace edge."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.reader :as reader]
            [katzen.program :as prog]
            [katzen.diagram :as diagram]
            ["@xyflow/react" :refer [ReactFlow Background Controls MiniMap Handle Position
                                     applyNodeChanges applyEdgeChanges]]
            ["elkjs/lib/elk.bundled.js" :default ELK]))

(def elk (ELK.))

;; ---------------------------------------------------------------------------
;; ELK layout — hierarchy-aware, so cond/fn groups nest as sub-flows
;; ---------------------------------------------------------------------------

(defn- ->elk [nodes edges]
  (let [by-parent (group-by :parentId nodes)
        node->elk (fn node->elk [n]
                    (let [kids (get by-parent (:id n))
                          base {:id (:id n) :width 120 :height 40}]
                      (if (seq kids)
                        (assoc base
                               :children (mapv node->elk kids)
                               :layoutOptions {"elk.algorithm" "layered"
                                               "elk.direction" "RIGHT"
                                               "elk.padding" "[top=30,left=14,bottom=14,right=14]"})
                        base)))]
    #js {:id "root"
         :layoutOptions #js {"elk.algorithm" "layered"
                             "elk.direction" "RIGHT"
                             "elk.hierarchyHandling" "INCLUDE_CHILDREN"
                             "elk.layered.spacing.nodeNodeBetweenLayers" "70"
                             "elk.spacing.nodeNode" "34"}
         :children (clj->js (mapv node->elk (get by-parent nil)))
         :edges (clj->js (mapv (fn [e] {:id (:id e) :sources [(:source e)] :targets [(:target e)]})
                               edges))}))

(defn- collect-pos [acc node]
  (let [acc (assoc acc (.-id node) {:x (.-x node) :y (.-y node)
                                    :w (.-width node) :h (.-height node)})]
    (reduce collect-pos acc (or (.-children node) #js []))))

;; ---------------------------------------------------------------------------
;; Custom node — Dusko's marks (multiple input ports; bead on the output port)
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
     (if pure? {:border "2px solid #0f172a"} {}))))   ; default box; bordered if pure

(defn- in-handle-style [n i]
  {:top (str (js/Math.round (* (/ (inc i) (inc n)) 100)) "%")
   :background "#94a3b8" :width 7 :height 7 :border "none"})

(defn- out-handle-style [pure?]
  (if pure?
    {:background "#0f172a" :width 11 :height 11 :border "2px solid #fff"}   ; cartesian bead •
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
  ;; a sub-flow container (if-branch / fn body) with its label in the corner
  (r/as-element
   [:div {:style {:width "100%" :height "100%" :pointerEvents "none"}}
    [:span {:style {:position "absolute" :top 3 :left 8 :fontSize 10 :fontWeight 700
                    :letterSpacing "0.04em" :textTransform "uppercase" :color "#a78bfa"}}
     (aget (.-data props) "label")]]))

(def ^:private node-types
  ;; module-level const (ReactFlow re-renders if nodeTypes identity changes)
  (let [mk (fn [t] (fn [props] (render-node t props)))]
    #js {"box" (mk "box") "cond" (mk "cond") "program" (mk "program")
         "run" (mk "run") "recur" (mk "recur") "recursive" (mk "recursive")
         "input" (mk "input") "result" (mk "result")
         "group" (fn [props] (render-group props))}))

;; ---------------------------------------------------------------------------
;; ReactFlow node/edge data (positions from ELK)
;; ---------------------------------------------------------------------------

(defn- ->rf-nodes [nodes pos]
  (clj->js
   (for [{:keys [id type parentId data]} nodes
         :let [{:keys [x y w h]} (get pos id {:x 0 :y 0})]]
     (cond-> {:id id :type type :data (or data {:label id}) :position {:x x :y y}}
       (= "group" type) (assoc :style {:width w :height h
                                       :background "rgba(148,163,184,0.06)"
                                       :border "1px dashed #94a3b8" :borderRadius 8})
       parentId (assoc :parentId parentId :extent "parent")))))

(defn- ->rf-edges [edges]
  (clj->js
   (for [{:keys [id source target sourceHandle targetHandle trace?]} edges]
     (cond-> {:id id :source source :target target
              :sourceHandle sourceHandle :targetHandle targetHandle}
       trace? (assoc :animated true :label "↺"
                     :style {:stroke "#a855f7" :strokeWidth 1.5 :strokeDasharray "6 4"})))))

;; ---------------------------------------------------------------------------
;; State + recompute
;; ---------------------------------------------------------------------------

;; A control system as a DIAGRAM VALUE (the same shape `fn->diagram` produces).
;; Shows the renderer is category-agnostic: directed machines (sensor → controller
;; → plant, with feedback) land in the SAME diagrammatic substrate as code.
;; (Computed by hand here; running `oapply` live in the browser is future work.)
(def control-example
  {:name "uav-control"
   :inputs [{:port "e" :label "e"} {:port "d" :label "d"}]
   :boxes [{:id "in_e" :label "e (setpoint)" :input? true :ins [] :out "e" :group []}
           {:id "in_d" :label "d (setpoint)" :input? true :ins [] :out "d" :group []}
           {:id "sensor"     :label "sensor"     :ins ["theta" "e"] :out "sp" :group []}
           {:id "controller" :label "controller" :ins ["sp" "d"]    :out "c"  :group []}
           {:id "plant"      :label "plant"      :ins ["c"]         :out "theta" :group []}]
   :wires [{:from "theta" :to "sp" :trace? true}   ; plant output fed BACK to the sensor
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

(defonce state (r/atom {:code default-code :nodes #js [] :edges #js [] :err nil}))

(defn- def-form? [form]
  (and (seq? form) (contains? '#{defn defn- fn fn*} (first form))))

(defn render-diagram! [dia]
  (let [rf (diagram/->reactflow dia)]
    (-> (.layout elk (->elk (:nodes rf) (:edges rf)))
        (.then (fn [res]
                 (swap! state assoc
                        :nodes (->rf-nodes (:nodes rf) (reduce collect-pos {} (.-children res)))
                        :edges (->rf-edges (:edges rf))
                        :err nil)))
        (.catch (fn [e] (swap! state assoc :err (str "layout: " e)))))))

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
       [:br] "nested boxes = if / fn body · ↺ dashed = recursion / feedback (trace)"
       [:br] "diagrams follow "
       [:a {:href "https://link.springer.com/book/10.1007/978-3-031-34827-3"
            :target "_blank" :rel "noopener" :style {:color "#6366f1"}}
        "Pavlović, Programs as Diagrams (Springer 2023)"]]
      (when err [:pre {:style {:color "#b00" :padding "8px 12px" :margin 0 :fontSize 12}} err])]
     [:div {:style {:flex 1}}
      [:> ReactFlow {:nodes nodes :edges edges :nodeTypes node-types :fitView true :minZoom 0.15
                     ;; controlled state → apply drag/select changes so boxes are MOVABLE
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
