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
  "Forms not yet given diagrammatic structure — rendered as opaque boxes for now
   (`case` multi-way, etc.)."
  '#{case condp if-let when-let try catch finally throw
     quote var set! def -> ->> as-> doto})

(defn- cond->if
  "Desugar `(cond p1 e1 p2 e2 … :else d)` into nested `if`s."
  [clauses]
  (when (seq clauses)
    (let [[p e & more] clauses]
      (if (= :else p) e (list 'if p e (cond->if more))))))

(defn- fn-first-clause
  "From a `(fn name? …)` / `(fn* …)` form return [params body…] for its first
   arity (multi-arity falls to the first clause)."
  [form]
  (let [more (rest form)
        more (if (symbol? (first more)) (rest more) more)]   ; optional self-name
    (if (vector? (first more))
      [(first more) (rest more)]                             ; (fn [ps] body…)
      (let [[ps & body] (first more)] [ps body]))))          ; (fn* ([ps] body…) …)

;; ---------------------------------------------------------------------------
;; The functor — a stateful walk threading an env {symbol → out-port}.
;; `:group` is the current branch path (a vector of [cond-id guard]); boxes
;; record it so the renderer can nest each conditional branch as a sub-diagram
;; (the operadic "fill"). Top-level boxes have group [].
;; ---------------------------------------------------------------------------

(defn- fresh [state] (let [n (:counter state)] [(update state :counter inc) (str "p" n)]))

(defn- add-box
  "Add a box; returns [state out-port box]. Box records the current `:group`,
   plus optional `:kind`/`:control` (for the cond box)."
  [state {:keys [label pure? ins kind control]}]
  (let [[state out] (fresh state)
        box (cond-> {:id (str "b" (count (:boxes state))) :label label
                     :pure? (boolean pure?) :ins (vec ins)
                     :group (:group state []) :out out}
              kind    (assoc :kind kind)
              control (assoc :control control))]
    [(update state :boxes conj box) out box]))

(defn- wire [state from to] (update state :wires conj {:from from :to to}))

(declare walk walk-cond walk-fn walk-apply walk-loop walk-recur)

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

    (= 'do op)       (reduce (fn [[st _] e] (walk st e)) [state nil] args)
    (= 'if op)       (walk-cond state (first args) (second args) (nth args 2 nil))
    (= 'when op)     (recur state (list 'if (first args) (cons 'do (rest args))))
    (= 'when-not op) (recur state (list 'if (list 'not (first args)) (cons 'do (rest args))))
    (= 'cond op)     (recur state (cond->if args))
    (or (= 'fn op) (= 'fn* op)) (walk-fn state form)
    (= 'loop op)     (walk-loop state (first args) (rest args))
    (= 'recur op)    (walk-recur state args)

    ;; RECURSION (phase 5, rendered as a trace / feedback loop): a self-call to
    ;; the defn's own name. The fixpoint's back-edge is drawn to the fn inputs.
    (and (symbol? op) (= (str op) (:fn-name state)))
    (let [targets (:recur-targets state)
          [state in-ports] (walk-args state args)
          [state out] (add-box state {:label (str op) :kind :recursive :pure? false :ins in-ports})]
      [(reduce (fn [st [ip tp]]
                 (cond-> (wire st ip out)
                   tp (update :wires conj {:from ip :to tp :trace? true})))
               state (map vector in-ports targets))
       out])

    ;; HIGHER-ORDER application (Dusko: `(f x) = {f} x` = run on a program). The
    ;; op is a LOCAL bound to a program value → apply via a `run` box.
    (and (symbol? op) (contains? (:env state) op))
    (walk-apply state (get-in state [:env op]) args)

    ;; opaque special form (loop/recur/…): one box over the bound symbols it uses
    (special-forms op)
    (let [used (->> (tree-seq coll? seq form)
                    (filter symbol?) (filter (:env state)) distinct)
          [state in-ports] (walk-args state used)
          [state out] (add-box state {:label (str op) :pure? false :ins in-ports})]
      [(reduce (fn [st p] (wire st p out)) state in-ports) out])

    ;; op is an expression evaluating to a program (a fn-literal, `(comp …)`, …)
    ;; → run it on the args.
    (seq? op) (let [[state code] (walk state op)] (walk-apply state code args))

    ;; ordinary call to a named (global) op
    :else
    (let [[state in-ports] (walk-args state args)
          label (if (symbol? op) (str op) (pr-str op))
          [state out] (add-box state {:label label
                                      :pure? (and (symbol? op) (pure-op? op))
                                      :ins in-ports})]
      [(reduce (fn [st p] (wire st p out)) state in-ports) out])))

(defn- walk-apply
  "Apply a program `code` (a P-typed value port) to `args` via a `:run` box —
   Dusko's `{code} args`. The selected program runs on the arguments."
  [state code args]
  (let [[state arg-ports] (walk-args state args)
        ins (cons code arg-ports)
        [state out] (add-box state {:label "apply" :kind :run :pure? false :ins ins})]
    [(reduce (fn [st p] (wire st p out)) state ins) out]))

(defn- walk-loop
  "A `(loop [v init …] body)`: each loop var is a port (initialised by `init`);
   `:recur-targets` is rebound to those ports so a `recur` inside feeds back to
   them (the trace). The loop's value is the body's value."
  [state binds-vec body]
  (let [binds  (partition 2 binds-vec)
        outer  (:recur-targets state)
        [state targets]
        (reduce (fn [[st ts] [sym init]]
                  (let [[st p] (walk st init)] [(assoc-in st [:env sym] p) (conj ts p)]))
                [state []] binds)
        [state out] (reduce (fn [[st _] e] (walk st e))
                            [(assoc state :recur-targets targets) nil] body)]
    [(assoc state :recur-targets outer) out]))

(defn- walk-recur
  "A `(recur args…)`: a `:recur` box whose args feed BACK to the current
   `:recur-targets` (loop vars, or the fn params for self-recursion) via TRACE
   edges (`:trace? true`) — recursion as a feedback loop (the Kleene fixpoint)."
  [state args]
  (let [targets (:recur-targets state)
        [state arg-ports] (walk-args state args)
        [state out] (add-box state {:label "recur" :kind :recur :pure? false :ins arg-ports})]
    [(reduce (fn [st [ap tp]]
               (cond-> (wire st ap out)
                 tp (update :wires conj {:from ap :to tp :trace? true})))
             state (map vector arg-ports targets))
     out]))

(defn- walk-fn
  "A `(fn [ps] body…)` / `(fn* …)` literal becomes a `:program` box (a quoted
   program code ⌜·⌝, Dusko's element of P). Its body is walked into a nested
   sub-diagram (params = the program's inputs; free vars wire in as the closure);
   the box's out-port is the program code, consumed by a `:run` box on application."
  [state form]
  (let [parent     (:group state [])
        outer-env  (:env state)
        [state out prog] (add-box state {:label "fn" :kind :program :ins []})
        gpath      (conj parent [(:id prog) :body])
        [params body] (fn-first-clause form)
        params     (remove #{'&} params)
        state (reduce (fn [st p]
                        (let [[st port] (fresh st)]
                          (-> st (assoc-in [:env p] port)
                              (update :boxes conj {:id (str "fnin_" port) :label (str p)
                                                   :input? true :ins [] :out port :group gpath}))))
                      (assoc state :group gpath) params)
        [state bout] (reduce (fn [[st _] e] (walk st e)) [state nil] body)
        state (cond-> state bout (wire bout out))]
    ;; restore the outer env (params/inner lets don't leak) and group
    [(assoc state :group parent :env outer-env) out]))

(defn- walk-cond
  "An `(if c t e)`: a `:cond` box implementing Pavlović's LAZY branching (§3.6.1,
   Eq 3.31) `ift(c, ⌜then⌝, ⌜else⌝) = {{c}(⌜then⌝, ⌜else⌝)}` — `{c}` selects a
   branch *program code*, the outer `run` evaluates it; only one branch runs. The
   two branches are walked into grouped sub-diagrams (they ARE the program codes
   F/G). The condition wires in as control; each branch's result wires to the
   box's output (the selection). Pure monoidal computer — no operad, no
   coproduct; the branches being programs is the 'programs are data' thesis."
  [state c t e]
  (let [[state ctrl]          (walk state c)
        parent                (:group state [])
        [state out cond-box]  (add-box state {:label "if" :kind :cond :control ctrl :ins []})
        cid                   (:id cond-box)
        branch (fn [st guard expr]
                 (let [[st bout] (walk (assoc st :group (conj parent [cid guard])) expr)]
                   (wire st bout out)))
        state (-> state (branch :then t) (branch :else e) (assoc :group parent))]
    [(wire state ctrl out) out]))

(defn- walk
  "Walk an expression; return [state out-port]. A bound symbol resolves to its
   port (reuse fans out naturally); a free symbol is a program/var reference
   `⌜x⌝` (a `:program` value — e.g. `inc` passed to `map`); a literal is a const box."
  [state expr]
  (cond
    (and (symbol? expr) (contains? (:env state) expr)) [state (get-in state [:env expr])]
    (seq? expr)   (walk-call state expr)
    (symbol? expr) (add-box state {:label (str expr) :kind :program :pure? true :ins []})
    :else         (add-box state {:label (pr-str expr) :pure? true :ins []})))

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
                             {:id (str "in_" port) :label label :input? true
                              :ins [] :out port :group []})
                           inputs))
        ;; recursion targets = the param ports (a self-call / recur feeds back here)
        state (assoc state :fn-name (some-> fname str)
                     :recur-targets (mapv :port inputs))
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
