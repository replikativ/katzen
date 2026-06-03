(ns katzen.playground
  "Interactive string-diagram playground: paste a Clojure `defn`/`fn`, see its
   string diagram live (katzen.program → katzen.diagram/->reactflow → ELK layout
   → ReactFlow). The functor + emitter are the shared .cljc code; this ns is the
   browser shell (the rendering functor's interactive back end)."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.reader :as reader]
            [katzen.program :as prog]
            [katzen.diagram :as diagram]
            ["@xyflow/react" :refer [ReactFlow Background Controls MiniMap]]
            ["elkjs/lib/elk.bundled.js" :default ELK]))

(def elk (ELK.))

;; ---------------------------------------------------------------------------
;; ELK layout — hierarchy-aware, so cond/fn groups nest as sub-flows
;; ---------------------------------------------------------------------------

(defn- ->elk
  "ReactFlow graph (nodes with :parentId, edges) → an ELK graph (children nest
   by parentId; layered, left-to-right). Edges go on the lowest common parent;
   for simplicity we put them all at the root with INCLUDE_CHILDREN handling."
  [nodes edges]
  (let [by-parent (group-by :parentId nodes)
        node->elk (fn node->elk [n]
                    (let [kids (get by-parent (:id n))
                          base {:id (:id n) :width 130 :height 44}]
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
;; ReactFlow node styling — Dusko's marks (bead = pure; shapes by kind)
;; ---------------------------------------------------------------------------

(def ^:private kind->builtin
  {"input" "input" "result" "output" "group" "group"})

(defn- node-label [{:keys [data]}]
  (let [{:keys [label pure? fanout dropped?]} data]
    (str label
         (when pure? " •")                        ; cartesian bead
         (when (> (or fanout 0) 1) (str " ×" fanout)) ; reused (copy / fan-out)
         (when dropped? " ⏚"))))                  ; dropped binding (delete)

(defn- node-style [{:keys [type data]} {:keys [w h]}]
  (let [{:keys [pure?]} data
        base {:fontSize 12 :fontFamily "monospace" :borderRadius 6}]
    (case type
      "group"   (merge base {:width w :height h :background "rgba(99,102,241,0.06)"
                             :border "1px dashed #6366f1" :color "#6366f1" :fontWeight 600})
      "cond"    (merge base {:background "#fff7ed" :border "2px solid #f59e0b"})
      "program" (merge base {:background "#eef2ff" :border "1px solid #6366f1"})
      "run"     (merge base {:background "#ecfeff" :border "1px solid #06b6d4"})
      "input"   (merge base {:background "#f0fdf4" :border "1px solid #22c55e"})
      "result"  (merge base {:background "#fef2f2" :border "1px solid #ef4444"})
      (merge base {:background "#fff" :border (if pure? "2px solid #111" "1px solid #999")}))))

(defn- ->rf-nodes [nodes pos]
  (clj->js
   (for [{:keys [id type parentId] :as n} nodes
         :let [p (get pos id {:x 0 :y 0})]]
     (cond-> {:id id
              :type (get kind->builtin type "default")
              :data {:label (node-label n)}
              :position {:x (:x p) :y (:y p)}
              :sourcePosition "right" :targetPosition "left"
              :style (node-style n p)}
       parentId (assoc :parentId parentId :extent "parent")))))

;; ---------------------------------------------------------------------------
;; State + recompute
;; ---------------------------------------------------------------------------

(def default-code
  "(defn stats [xs]\n  (let [n      (count xs)\n        total  (reduce + 0 xs)\n        mean   (/ total n)\n        unused (first xs)]\n    (if (pos? n)\n      (vector mean n)\n      (vector 0 0))))")

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
                              :edges (clj->js (:edges rf))
                              :err nil)))
              (.catch (fn [e] (swap! state assoc :err (str "layout: " e))))))))
    (catch :default e (swap! state assoc :err (str "read: " (.-message e))))))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(defn app []
  (let [{:keys [code nodes edges err]} @state]
    [:div {:style {:display "flex" :height "100vh" :fontFamily "monospace"}}
     [:div {:style {:width "38%" :display "flex" :flexDirection "column" :borderRight "1px solid #e1e4e8"}}
      [:div {:style {:padding "8px 12px" :background "#f6f8fa" :borderBottom "1px solid #e1e4e8"
                     :fontWeight 600}}
       "katzen — Clojure as a string diagram"]
      [:textarea {:value code
                  :spellCheck false
                  :on-change #(let [v (.. % -target -value)]
                                (swap! state assoc :code v) (recompute! v))
                  :style {:flex 1 :border "none" :outline "none" :resize "none"
                          :padding "12px" :fontSize 13 :fontFamily "monospace" :tabSize 2}}]
      [:div {:style {:padding "6px 12px" :fontSize 11 :color "#586069" :borderTop "1px solid #e1e4e8"}}
       "• = pure (cartesian)   ⏚ = dropped binding   nested boxes = if/fn"]
      (when err [:pre {:style {:color "#b00" :padding "8px 12px" :margin 0 :fontSize 12}} err])]
     [:div {:style {:flex 1}}
      [:> ReactFlow {:nodes nodes :edges edges :fitView true :minZoom 0.2}
       [:> Background]
       [:> Controls]
       [:> MiniMap]]]]))

(defn init! []
  (recompute! default-code)
  (rdom/render [app] (js/document.getElementById "app")))
