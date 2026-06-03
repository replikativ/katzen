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

(def ^:private node-types
  ;; module-level const (ReactFlow re-renders if nodeTypes identity changes)
  (let [mk (fn [t] (fn [props] (render-node t props)))]
    #js {"box" (mk "box") "cond" (mk "cond") "program" (mk "program")
         "run" (mk "run") "recur" (mk "recur") "recursive" (mk "recursive")
         "input" (mk "input") "result" (mk "result")}))

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

(def default-code
  "(defn fact [n acc]\n  (if (pos? n)\n    (recur (dec n) (* n acc))\n    acc))")

(defonce state (r/atom {:code default-code :nodes #js [] :edges #js [] :err nil}))

(defn- def-form? [form]
  (and (seq? form) (contains? '#{defn defn- fn fn*} (first form))))

(defn recompute! [code]
  (try
    (let [form (reader/read-string code)]
      (if-not (def-form? form)
        (swap! state assoc :err "Paste a (defn …) or (fn …) form.")
        (let [rf (diagram/->reactflow (prog/fn->diagram form))]
          (-> (.layout elk (->elk (:nodes rf) (:edges rf)))
              (.then (fn [res]
                       (swap! state assoc
                              :nodes (->rf-nodes (:nodes rf) (reduce collect-pos {} (.-children res)))
                              :edges (->rf-edges (:edges rf))
                              :err nil)))
              (.catch (fn [e] (swap! state assoc :err (str "layout: " e))))))))
    (catch :default e (swap! state assoc :err (str "read: " (.-message e))))))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(defn app []
  (let [{:keys [code nodes edges err]} @state]
    [:div {:style {:display "flex" :height "100vh" :fontFamily "monospace"}}
     [:div {:style {:width "36%" :display "flex" :flexDirection "column" :borderRight "1px solid #e1e4e8"}}
      [:div {:style {:padding "8px 12px" :background "#f6f8fa" :borderBottom "1px solid #e1e4e8" :fontWeight 600}}
       "katzen — Clojure as a string diagram"]
      [:textarea {:value code :spellCheck false
                  :on-change #(let [v (.. % -target -value)]
                                (swap! state assoc :code v) (recompute! v))
                  :style {:flex 1 :border "none" :outline "none" :resize "none"
                          :padding "12px" :fontSize 13 :fontFamily "monospace" :tabSize 2}}]
      [:div {:style {:padding "6px 12px" :fontSize 11 :color "#586069" :borderTop "1px solid #e1e4e8" :lineHeight 1.6}}
       "● on output = pure (cartesian) · multiple ports = multiple args · ⏚ = dropped binding"
       [:br] "nested boxes = if / fn body · ↺ dashed = recursion (trace)"]
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
