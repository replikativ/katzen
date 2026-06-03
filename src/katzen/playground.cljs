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

(defn- descendants-of
  "Set of node ids strictly under group `g` (following the parentId chain)."
  [pmap g]
  (set (for [id (keys pmap)
             :when (loop [p (get pmap id)]
                     (cond (nil? p) false (= p g) true :else (recur (get pmap p))))]
         id)))

(defn- group-subdiagram
  "Intra-function drill-down: the sub-diagram *inside* group `g`, re-rooted as a
   standalone graph. Wires crossing the boundary become synthetic ports — Dusko's
   sub-diagram has input ports (the in-scope values it uses) and an output port."
  [rf g]
  (let [{:keys [nodes edges]} rf
        pmap     (parent-map nodes)
        by-id    (into {} (map (juxt :id identity) nodes))
        label-of #(get-in by-id [% :data :label] %)
        inside   (descendants-of pmap g)
        ;; the group's own contents, with direct children re-rooted to the top
        focus-nodes (for [{:keys [id parentId] :as n} nodes :when (inside id)]
                      (cond-> n (= parentId g) (dissoc :parentId)))
        cross?  (fn [e dir] (case dir
                              :in  (and (inside (:target e)) (not (inside (:source e))))
                              :out (and (inside (:source e)) (not (inside (:target e))))))
        inner   (filter #(and (inside (:source %)) (inside (:target %))) edges)
        ins     (filter #(cross? % :in) edges)
        outs    (filter #(cross? % :out) edges)
        in-ports  (for [s (distinct (map :source ins))]
                    {:id (str "bin_" s) :type "input"
                     :data {:label (str (label-of s)) :inputs 0 :boundary? true}})
        out-ports (for [t (distinct (map :target outs))]
                    {:id (str "bout_" t) :type "result"
                     :data {:label (str "→ " (label-of t)) :boundary? true}})
        in-edges  (for [e ins]  (assoc e :id (str "bin_" (:source e) "->" (:target e))
                                       :source (str "bin_" (:source e)) :sourceHandle "out"))
        out-edges (for [e outs] (assoc e :id (str (:source e) "->bout_" (:target e))
                                       :target (str "bout_" (:target e)) :targetHandle "in-0"))]
    {:label (str (label-of g))
     :rf {:nodes (concat in-ports focus-nodes out-ports)
          :edges (concat in-edges inner out-edges)}}))

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
        expand?  (aget d "expandable?")
        n        (max 1 (or (aget d "inputs") 0))]
    (r/as-element
     [:div {:style (cond-> (node-css t pure?)
                     expand? (assoc :border "1.5px solid #6366f1" :cursor "pointer"))
            :title (when expand? (str "double-click to open " label))}
      (map (fn [i]
             [:> Handle {:key (str "in" i) :type "target" :position (.-Left Position)
                         :id (str "in-" i) :style (in-handle-style n i)}])
           (range n))
      [:span {:style {:whiteSpace "nowrap"}} label (when dropped? " ⏚")
       (when expand? [:span {:style {:color "#6366f1" :marginLeft 4 :fontWeight 700}} "⊞"])]
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

;; :rf      — the base (top-level fn) diagram graph {:nodes :edges}
;; :corpus  — {fn-name(str) → defn-form}, parsed from the textarea (drill targets)
;; :focus   — a stack of drilled-in frames {:label str :rf {:nodes :edges}}; the
;;            rendered diagram is the top frame's, or :rf when the stack is empty.
;;            (Dusko: drilling a call box = run {⌜f⌝}; the stack is the run-stack.)
(defonce state (r/atom {:code "" :rf nil :dia nil :view :reactflow
                        :corpus {} :focus []
                        :collapsed #{} :nodes #js [] :edges #js [] :err nil}))

(defn- current-rf
  "The diagram graph currently in view: the top focus frame, else the base."
  [{:keys [rf focus]}]
  (if (seq focus) (:rf (peek focus)) rf))

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

(defn- ->rf-nodes [vis-nodes collapsed pos drillable]
  (clj->js
   (for [{:keys [id type parentId data]} vis-nodes
         :let [{:keys [x y w h]} (get pos id {:x 0 :y 0})
               collapsed? (contains? collapsed id)
               ;; a plain call box whose label names a known fn → drillable (⊞)
               expandable? (and (= "box" type) (contains? drillable (:label data)))]]
     (cond-> {:id id :type type
              :data (assoc (or data {:label id}) :collapsed collapsed? :expandable? expandable?)
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
  (let [{:keys [collapsed corpus] :as s} @state
        rf (current-rf s)]
    (when rf
      (let [nodes (:nodes rf) edges (:edges rf)
            pmap  (parent-map nodes)
            vis   (remove #(hidden? pmap collapsed (:id %)) nodes)
            drillable (set (keys corpus))]
        (-> (.layout elk (->elk vis pmap collapsed edges))
            (.then (fn [res]
                     (swap! state assoc
                            :nodes (->rf-nodes vis collapsed (reduce collect-pos {} (.-children res)) drillable)
                            :edges (->rf-edges edges pmap collapsed)
                            :err nil)))
            (.catch (fn [e] (swap! state assoc :err (str "layout: " e)))))))))

(defn toggle-collapse! [id]
  (swap! state update :collapsed #(if (contains? % id) (disj % id) (conj % id)))
  (layout!))

;; ---------------------------------------------------------------------------
;; Drill-down navigation — a focus stack with a breadcrumb (Navigate model).
;; Intra-function: step into an if/fn GROUP. Inter-function: step into a CALL
;; box that names a known defn (= evaluate {⌜f⌝} on demand; Dusko §2 run/encode).
;; ---------------------------------------------------------------------------

(defn drill-into-group! [id]
  (when-let [frame (group-subdiagram (current-rf @state) id)]
    (swap! state #(-> % (update :focus conj frame) (assoc :collapsed #{})))
    (layout!)))

(defn drill-into-fn! [fname]
  (when-let [form (get-in @state [:corpus fname])]
    (swap! state #(-> % (update :focus conj {:label fname
                                             :rf (diagram/->reactflow (prog/fn->diagram form))})
                      (assoc :collapsed #{})))
    (layout!)))

(defn focus-to!
  "Breadcrumb click: keep the first `n` focus frames (n=0 ⇒ back to the base)."
  [n]
  (swap! state #(-> % (assoc :focus (vec (take n (:focus %)))) (assoc :collapsed #{})))
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
   {:label "variance — inter-function (drill ⊞ into mean / sq)"
    :code "(defn variance [xs]\n  (let [m (mean xs)]\n    (/ (reduce + 0 (map (fn [x] (sq (- x m))) xs))\n       (count xs))))\n\n(defn mean [xs]\n  (/ (reduce + 0 xs) (count xs)))\n\n(defn sq [x] (* x x))"}
   {:label "UAV control loop — directed machines (dynamics)"
    :diagram control-example
    :code ";; A CONTROL SYSTEM, not Clojure code — the SAME diagram substrate.\n;; sensor → controller → plant, with the plant output fed back to the\n;; sensor (the ↺ trace). Outer inputs: setpoints e, d.  (See doc/composition.md.)"}])

(def default-code (:code (first examples)))

(defn- def-form? [form]
  (and (seq? form) (contains? '#{defn defn- fn fn*} (first form))))

(defn- fn-name-of [form]
  (let [[h n] form] (when (and (contains? '#{defn defn-} h) (symbol? n)) (str n))))

(defn render-diagram! [dia]
  (swap! state assoc :dia dia :rf (diagram/->reactflow dia) :collapsed #{} :focus [])
  (layout!))

(defn recompute! [code]
  ;; Read ALL top-level forms (a mini-namespace), keep the defns as a corpus of
  ;; drill targets, render the first as the entry point.
  (try
    (let [forms (reader/read-string (str "[" code "\n]"))
          defs  (filter def-form? forms)]
      (if (empty? defs)
        (swap! state assoc :err "Paste one or more (defn …) / (fn …) forms.")
        (let [corpus (into {} (keep #(when-let [nm (fn-name-of %)] [nm %]) defs))]
          (swap! state assoc :corpus corpus :err nil)
          (render-diagram! (prog/fn->diagram (first defs))))))
    (catch :default e (swap! state assoc :err (str "read: " (.-message e))))))

(defn load-example! [{:keys [code diagram]}]
  (swap! state assoc :code code :err nil)
  (if diagram
    (do (swap! state assoc :corpus {}) (render-diagram! diagram))
    (recompute! code)))

;; ---------------------------------------------------------------------------
;; Mermaid view — the SECOND rendering functor off the same diagram value.
;; Conventional flowchart (auto-layout, read-only), and — unlike ReactFlow —
;; the source string embeds directly in markdown (GitHub/Notion render it).
;; ---------------------------------------------------------------------------

;; mermaid is large and pulls a diagram-type graph shadow-cljs can't bundle, so
;; it's loaded from CDN by a <script type=module> in the HTML, which sets
;; window.mermaid (already initialized). We just wait for it to appear.
(defn- load-mermaid!
  "Promise resolving to the mermaid object once the CDN module has loaded."
  []
  (js/Promise.
   (fn [resolve reject]
     (letfn [(check [n]
               (if-let [m (.-mermaid js/window)]
                 (resolve m)
                 (if (> n 120)
                   (reject (js/Error. "mermaid failed to load from CDN"))
                   (js/setTimeout #(check (inc n)) 50))))]
       (check 0)))))

(defn- mermaid-view
  "Renders `code` (a mermaid flowchart string) to SVG. Re-renders on prop change."
  [_code]
  (let [svg     (r/atom "")
        last    (atom ::none)          ; guard: render only when the code changes
        render! (fn [c]
                  (when (not= c @last)
                    (reset! last c)
                    (-> (load-mermaid!)
                        (.then  (fn [m] (.render m (str "m" (js/Math.abs (hash c))) c)))
                        (.then  (fn [res] (reset! svg (.-svg res))))
                        (.catch (fn [e] (reset! svg (str "<pre style='color:#b00;padding:12px'>"
                                                         (.-message e) "</pre>")))))))]
    (r/create-class
     {:display-name         "mermaid-view"
      :component-did-mount   (fn [this] (render! (nth (r/argv this) 1)))
      :component-did-update  (fn [this _] (render! (nth (r/argv this) 1)))
      :reagent-render
      (fn [_code]
        [:div {:style {:width "100%" :height "100%" :overflow "auto"
                       :padding 16 :boxSizing "border-box" :textAlign "center"}
               :dangerouslySetInnerHTML #js {:__html @svg}}])})))

(defn- copy! [s]
  (-> (.. js/navigator -clipboard (writeText s)) (.catch (fn [_] nil))))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(defn- view-tab [view label active?]
  [:button {:on-click #(swap! state assoc :view view)
            :style {:fontFamily "inherit" :fontSize 11 :padding "3px 10px" :cursor "pointer"
                    :border "1px solid #cbd5e1" :borderRadius 5
                    :background (if active? "#0f172a" "#fff") :color (if active? "#fff" "#334155")}}
   label])

(defn- breadcrumb [dia focus]
  (let [base   (or (:name dia) "fn")
        labels (cons base (map :label focus))
        n      (count focus)]
    [:div {:style {:padding "5px 12px" :borderBottom "1px solid #eef0f2" :background "#fff"
                   :fontSize 11 :display "flex" :alignItems "center" :flexWrap "wrap"}}
     (map-indexed
      (fn [i lbl]
        [:span {:key i :style {:display "inline-flex" :alignItems "center"}}
         (when (pos? i) [:span {:style {:color "#cbd5e1" :margin "0 5px"}} "›"])
         [:span {:on-click #(focus-to! i) :title "go to this level"
                 :style {:cursor "pointer" :userSelect "none"
                         :fontWeight (if (= i n) 700 400)
                         :color (if (= i n) "#0f172a" "#6366f1")}}
          lbl]])
      labels)
     (when (zero? n)
       [:span {:style {:marginLeft 8 :color "#9aa4b2" :fontStyle "italic"}}
        "— double-click a ⊞ box or a group to drill in"])]))

(defn- on-node-dblclick [_ ^js node]
  (let [d (.-data node)]
    (cond
      (= "group" (.-type node)) (drill-into-group! (.-id node))
      (aget d "expandable?")    (drill-into-fn! (aget d "label")))))

(defn app []
  (let [{:keys [code nodes edges err view dia focus]} @state
        mcode (when dia (diagram/->mermaid dia))]
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
     [:div {:style {:flex 1 :display "flex" :flexDirection "column"}}
      [:div {:style {:padding "6px 12px" :borderBottom "1px solid #e1e4e8" :background "#fafbfc"
                     :display "flex" :alignItems "center" :gap 6}}
       [view-tab :reactflow "◆ string diagram" (= view :reactflow)]
       [view-tab :mermaid "▤ mermaid" (= view :mermaid)]
       [:span {:style {:flex 1}}]
       (when (= view :mermaid)
         [:button {:on-click #(copy! mcode) :title "copy mermaid source (paste into any markdown)"
                   :style {:fontFamily "inherit" :fontSize 11 :padding "3px 10px" :cursor "pointer"
                           :border "1px solid #cbd5e1" :borderRadius 5 :background "#fff" :color "#334155"}}
          "⧉ copy source"])]
      (when (= view :reactflow) [breadcrumb dia focus])
      [:div {:style {:flex 1 :minHeight 0}}
       (if (= view :mermaid)
         (if mcode ^{:key mcode} [mermaid-view mcode]
             [:div {:style {:padding 16 :color "#586069"}} "No diagram."])
         [:> ReactFlow {:nodes nodes :edges edges :nodeTypes node-types :fitView true :minZoom 0.15
                        :onNodeDoubleClick on-node-dblclick
                        :onNodesChange (fn [changes]
                                         (swap! state update :nodes #(applyNodeChanges changes %)))
                        :onEdgesChange (fn [changes]
                                         (swap! state update :edges #(applyEdgeChanges changes %)))}
          [:> Background]
          [:> Controls]
          [:> MiniMap {:pannable true :zoomable true}]])]]]))

(defn init! []
  (recompute! default-code)
  (rdom/render [app] (js/document.getElementById "app")))
