(ns katzen.program
  "Clojure programs as string diagrams — `⟦·⟧ : fn-body → wiring diagram` (doc
   programs-as-diagrams.md). The Clojure analog of Catlab's `@program`
   (ParseJuliaPrograms), and like it, general categorical machinery that belongs
   in the framework (not in a downstream app).

   Grounded in Pavlović's monoidal computer: a function body becomes a wiring
   diagram where WIRES are values (no variables), BOXES are operations, a value
   used more than once FANS OUT (copy `Δ`), an unused binding is DROPPED (delete
   `▪`), and pure/total boxes carry a cartesian BEAD (which is what licenses CSE /
   dead-code elimination). `let` is wiring, not an environment.

   Scope (phase 2): `defn`/`fn` + `let` + nested calls + symbols + literals — the
   pure cartesian dataflow. Control / higher-order / recursion (`if`, `fn`-as-
   value, `loop`/`recur`) render as OPAQUE boxes for now (they need the cond-box /
   `run`-on-P / trace structure of later phases); the functor degrades gracefully
   rather than failing on real code.

   Returns the data of an attributed directed wiring diagram (see
   `katzen.diagram` for the shape and rendering). Backing it with the
   `katzen.dwd` ACSet — so it composes via `oapply` and shares the renderer with
   dynamics diagrams — is the monoidal-layer phase.

   Zero extra deps: a structural walk over Clojure forms (no analyzer/SCI). The
   purity test here is a syntactic stand-in for a real effect analysis."
  (:require [clojure.string :as str]
            [katzen.diagram :as diagram]))

(def ^:private impure
  '#{atom swap! reset! deref println print pr prn print-str newline flush
     send send-off alter ref-set dosync rand rand-int rand-nth shuffle
     slurp spit read read-line eval require load swap-vals! reset-vals!})

(defn- pure-op? [sym]
  (not (or (contains? impure sym) (str/ends-with? (name sym) "!"))))

(def ^:private special-forms
  '#{if when when-not when-let if-let cond condp case do fn fn* loop recur
     try catch finally throw quote var set! def -> ->> as-> doto})

;; ---------------------------------------------------------------------------
;; The functor — a stateful walk threading an env {symbol → out-port}
;; ---------------------------------------------------------------------------

(defn- fresh [state] (let [n (:counter state)] [(update state :counter inc) (str "p" n)]))

(defn- add-box [state {:keys [label pure? ins]}]
  (let [[state out] (fresh state)
        box {:id (str "b" (count (:boxes state))) :label label
             :pure? (boolean pure?) :ins (vec ins) :out out}]
    [(update state :boxes conj box) out]))

(defn- wire [state from to] (update state :wires conj {:from from :to to}))

(declare walk)

(defn- walk-args [state args]
  (reduce (fn [[st ports] a] (let [[st p] (walk st a)] [st (conj ports p)]))
          [state []] args))

(defn- walk-call
  [state [op & args :as form]]
  (cond
    (= 'let (first form)) (recur state (cons 'let* (rest form)))

    ;; let — pure wiring: bind each name to its expr's out-port, then the body
    (= 'let* op)
    (let [binds (partition 2 (first args))
          state (reduce (fn [st [sym expr]]
                          (let [[st p] (walk st expr)] (assoc-in st [:env sym] p)))
                        state binds)]
      (reduce (fn [[st _] e] (walk st e)) [state nil] (rest args)))

    ;; opaque special form (if/fn/loop/…): one box over the bound symbols it uses
    (special-forms op)
    (let [used (->> (tree-seq coll? seq form)
                    (filter symbol?) (filter (:env state)) distinct)
          [state in-ports] (walk-args state used)
          [state out] (add-box state {:label (str op) :pure? false :ins in-ports})]
      [(reduce (fn [st p] (wire st p out)) state in-ports) out])

    ;; ordinary call op(args…)
    :else
    (let [[state in-ports] (walk-args state args)
          label (if (symbol? op) (str op) (pr-str op))
          [state out] (add-box state {:label label
                                      :pure? (and (symbol? op) (pure-op? op))
                                      :ins in-ports})]
      [(reduce (fn [st p] (wire st p out)) state in-ports) out])))

(defn- walk
  "Walk an expression; return [state out-port]. A bound symbol resolves to its
   port (reuse fans out naturally); a literal/free symbol becomes a 0-input box."
  [state expr]
  (cond
    (and (symbol? expr) (contains? (:env state) expr)) [state (get-in state [:env expr])]
    (seq? expr) (walk-call state expr)
    :else (add-box state {:label (pr-str expr) :pure? true :ins []})))

(defn fn->diagram
  "Project a `(defn name [params] body…)` or `(fn [params] body…)` form into a
   wiring-diagram value `{:name :inputs :boxes :wires :outputs}` (the shape
   `katzen.diagram` renders)."
  [form]
  (let [[head & more] form
        named?  (and (= 'defn head) (symbol? (first more)))
        fname   (when named? (first more))
        more    (if named? (rest more) more)
        more    (drop-while (some-fn string? map?) more)          ; docstring / attr-map
        params  (remove #{'&} (first more))
        body    (rest more)
        [state inputs]
        (reduce (fn [[st ins] p]
                  (let [[st port] (fresh st)]
                    [(assoc-in st [:env p] port) (conj ins {:port port :label (str p)})]))
                [{:counter 0 :boxes [] :wires [] :env {}} []] params)
        ;; render the inputs as source boxes too
        state (update state :boxes into
                      (map (fn [{:keys [port label]}]
                             {:id (str "in_" port) :label label :input? true :ins [] :out port})
                           inputs))
        [state out] (reduce (fn [[st _] e] (walk st e)) [state nil] body)]
    {:name (some-> fname str)
     :inputs inputs
     :boxes (:boxes state)
     :wires (vec (distinct (:wires state)))
     :outputs (when out [out])}))

(defn fn->mermaid
  "Convenience: a `defn`/`fn` form → mermaid string (via `katzen.diagram`)."
  [form]
  (diagram/->mermaid (fn->diagram form)))
