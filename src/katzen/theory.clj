(ns katzen.theory
  "Theory macro for defining Generalized Algebraic Theories.

  This provides a Clojure DSL for defining GATs that mirrors GATlab.jl's
  @theory macro. The syntax is designed to be clear and idiomatic in Clojure
  while maintaining compatibility with GATlab's semantics.

  Supports both ASCII and unicode syntax following GATlab.jl conventions:
    - compose or ⋅ (center dot)
    - Hom or → (arrow)
    - otimes or ⊗ (tensor)
    - munit or I (monoidal unit)

  Example:
    (deftheory Category
      (type Ob)
      (type Hom [dom Ob, codom Ob])
      (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))
      (axiom assoc
        :ctx [a Ob, b Ob, c Ob, d Ob,
              f (Hom a b), g (Hom b c), h (Hom c d)]
        (= (compose (compose f g) h)
           (compose f (compose g h)))))"
  (:require [katzen.core :as core]
            [katzen.scope :as scope]
            [katzen.unicode :as unicode]
            [clojure.string :as str]))

;;; ============================================================================
;;; Parser State
;;; ============================================================================

(defrecord ParserState [gat scope-ctx type-env])

(defn fresh-parser-state
  "Create a fresh parser state for a new theory."
  [theory-name]
  (let [tag (scope/scope-tag)
        gat (core/gat theory-name tag [core/TYPE] [] [] [])]
    (->ParserState gat (scope/scope-context tag) {})))

;;; ============================================================================
;;; Type Expression Parsing
;;; ============================================================================

;; Forward declaration for mutual recursion
(declare parse-term-expr)

(defn- is-term-constructor?
  "Check if an ident refers to a term constructor in the GAT."
  [gat ident]
  (some #(= ident (-> % :term :head))
        (:term-constructors gat)))

(defn parse-term-application-in-type
  "Helper to recursively parse term applications appearing as type arguments.
  Returns [state AlgTerm]."
  [state expr]
  (let [[head & args] expr
        normalized-head (unicode/normalize-name head)]
    (if-let [head-ident (scope/lookup (:scope-ctx state) normalized-head)]
      (let [[final-state parsed-args]
            (reduce
             (fn [[s a] arg]
               (cond
                 ;; Symbol argument - could be an ident OR a nullary term (normalize unicode)
                 (symbol? arg)
                 (let [normalized-arg (unicode/normalize-name arg)]
                   (if-let [arg-ident (scope/lookup (:scope-ctx s) normalized-arg)]
                     ;; Check if this is a term constructor (nullary term application)
                     (if (is-term-constructor? (:gat s) arg-ident)
                       ;; It's a nullary term - create AlgTerm with no args
                       (let [nullary-type (core/alg-type arg-ident [] core/TYPE)
                             nullary-term (core/alg-term arg-ident [] nullary-type)]
                         [s (conj a nullary-term)])
                       ;; It's a regular ident
                       [s (conj a arg-ident)])
                     (throw (ex-info (str "Unbound variable in term application: " arg)
                                     {:arg arg :expr expr}))))

                 ;; Nested term application - recurse
                 (seq? arg)
                 (let [[new-s nested-term] (parse-term-application-in-type s arg)]
                   [new-s (conj a nested-term)])

                 :else
                 (throw (ex-info (str "Invalid argument in term application: " arg)
                                 {:arg arg :expr expr}))))
             [state []]
             args)
            ;; Create placeholder type for the term
            placeholder-type (core/alg-type head-ident [] core/TYPE)
            term-result (core/alg-term head-ident parsed-args placeholder-type)]
        [final-state term-result])
      (throw (ex-info (str "Unbound term constructor: " head)
                      {:head head :expr expr})))))

(defn parse-type-expr
  "Parse a type expression in the given context.

  Type expressions can be:
  - Symbol: Type variable reference (e.g., Ob)
  - List: Type application (e.g., (Hom a b))

  Returns [state AlgType]"
  [state expr]
  (cond
    ;; Type variable: just a symbol (normalize unicode)
    (symbol? expr)
    (let [normalized-expr (unicode/normalize-name expr)]
      (if-let [ident (scope/lookup (:scope-ctx state) normalized-expr)]
        [state (core/alg-type ident [] core/TYPE)]
        (throw (ex-info (str "Unbound type variable: " expr)
                        {:expr expr :normalized normalized-expr}))))

    ;; Type application: (Head arg1 arg2 ...) - normalize head
    (seq? expr)
    (let [[head & args] expr
          normalized-head (unicode/normalize-name head)]
      (if-let [head-ident (scope/lookup (:scope-ctx state) normalized-head)]
        ;; Parse each argument - can be symbols, type expressions, or term applications
        (let [[state arg-values]
              (reduce
               (fn [[st acc] arg]
                 (cond
                   ;; Symbol argument - look it up
                   (symbol? arg)
                   (if-let [arg-ident (scope/lookup (:scope-ctx st) arg)]
                     [st (conj acc arg-ident)]
                     (throw (ex-info (str "Unbound argument: " arg)
                                     {:arg arg :expr expr})))

                   ;; Nested expression - parse as a term application
                   ;; (e.g., (otimes a b) when used as a type argument in (Hom (otimes a b) ...))
                   (seq? arg)
                   (let [[new-st term-result] (parse-term-application-in-type st arg)]
                     [new-st (conj acc term-result)])

                   :else
                   (throw (ex-info (str "Invalid type argument: " arg)
                                   {:arg arg :expr expr}))))
               [state []]
               args)]
          [state (core/alg-type head-ident arg-values core/TYPE)])
        (throw (ex-info (str "Unbound type constructor: " head)
                        {:head head :expr expr}))))

    :else
    (throw (ex-info (str "Invalid type expression: " expr)
                    {:expr expr}))))

;;; ============================================================================
;;; Context Parsing
;;; ============================================================================

(defn parse-binding
  "Parse a single binding like [x T] where x is a name and T is a type.

  Returns [state ident type] where ident is the bound identifier."
  [state [name type-expr]]
  (when-not (symbol? name)
    (throw (ex-info (str "Binding name must be a symbol, got: " name)
                    {:name name})))
  (let [[state type] (parse-type-expr state type-expr)
        [new-scope-ctx ident] (scope/bind (:scope-ctx state) name)
        new-state (assoc state :scope-ctx new-scope-ctx)]
    [new-state ident type]))

(defn parse-context
  "Parse a context from a vector of bindings.

  Input: [[x T] [y S] ...]
  Returns: [state TypeCtx]

  Optionally accepts an initial context to extend."
  ([state bindings]
   (parse-context state bindings (core/type-ctx)))
  ([state bindings initial-ctx]
   (reduce
    (fn [[st ctx] binding]
      (let [[new-st ident type] (parse-binding st binding)]
        [new-st (core/add-binding ctx ident type)]))
    [state initial-ctx]
    bindings)))

;;; ============================================================================
;;; Term Expression Parsing
;;; ============================================================================

(defn parse-term-expr
  "Parse a term expression with a given expected type.

  Term expressions can be:
  - Symbol: Variable reference
  - List: Function application (term-constructor arg1 arg2 ...)

  Returns [state AlgTerm]"
  [state expr expected-type]
  (cond
    ;; Variable reference
    (symbol? expr)
    (if-let [ident (scope/lookup (:scope-ctx state) expr)]
      [state (core/alg-term ident [] expected-type)]
      (throw (ex-info (str "Unbound variable: " expr)
                      {:expr expr})))

    ;; Function application
    (seq? expr)
    (let [[head & args] expr
          normalized-head (unicode/normalize-name head)]
      (if-let [head-ident (scope/lookup (:scope-ctx state) normalized-head)]
        ;; Parse arguments (can be symbols or nested term expressions)
        (let [[state arg-terms]
              (reduce
               (fn [[st acc] arg]
                 (cond
                   ;; Symbol argument - just look it up
                   (symbol? arg)
                   (if-let [arg-ident (scope/lookup (:scope-ctx st) arg)]
                     [st (conj acc arg-ident)]
                     (throw (ex-info (str "Unbound term argument: " arg)
                                     {:arg arg :expr expr})))

                   ;; Nested term expression - recursively parse it
                   (seq? arg)
                   (let [[new-st arg-term] (parse-term-expr st arg expected-type)]
                     [new-st (conj acc arg-term)])

                   :else
                   (throw (ex-info (str "Term argument must be symbol or expression: " arg)
                                   {:arg arg :expr expr}))))
               [state []]
               args)]
          [state (core/alg-term head-ident arg-terms expected-type)])
        (throw (ex-info (str "Unbound term constructor: " head)
                        {:head head :expr expr}))))

    :else
    (throw (ex-info (str "Invalid term expression: " expr)
                    {:expr expr}))))

;;; ============================================================================
;;; Declaration Parsing
;;; ============================================================================

(defn parse-type-decl
  "Parse a type declaration.

  Forms:
    (type Ob)                           ; Nullary type
    (type Hom [dom Ob, codom Ob])       ; Type with context
    (type → [dom Ob, codom Ob])         ; Unicode alias for Hom"
  [state [_type type-name & rest]]
  (when-not (symbol? type-name)
    (throw (ex-info "Type name must be a symbol"
                    {:name type-name})))

  ;; Normalize unicode operators to ASCII
  (let [type-name (unicode/normalize-name type-name)
        ;; Check if there's a context (rest is a list, first element might be the vector)
        ctx-bindings (when (seq rest) (first rest))]
    (if (and ctx-bindings (vector? ctx-bindings))
      ;; Type with context
      (let [;; Parse bindings in context (commas are whitespace in Clojure, so no need to remove)
            bindings (partition 2 ctx-bindings)
            [state ctx] (parse-context state bindings)
            ;; Create ident for the type constructor
            [new-scope-ctx type-ident] (scope/bind (:scope-ctx state) type-name)
            ;; Create the type (type constructor takes context vars as args)
            type (core/alg-type type-ident (vec (:idents ctx)) core/TYPE)
            tic (core/type-in-ctx ctx type)
            ;; Add to GAT
            new-gat (core/add-type-constructor (:gat state) tic)
            new-state (-> state
                          (assoc :scope-ctx new-scope-ctx)
                          (assoc :gat new-gat))]
        new-state)
      ;; Nullary type
      (let [[new-scope-ctx type-ident] (scope/bind (:scope-ctx state) type-name)
            type (core/alg-type type-ident [] core/TYPE)
            tic (core/type-in-ctx (core/type-ctx) type)
            new-gat (core/add-type-constructor (:gat state) tic)]
        (-> state
            (assoc :scope-ctx new-scope-ctx)
            (assoc :gat new-gat))))))

(defn parse-term-decl
  "Parse a term declaration.

  Form:
    (term compose
      :ctx [a Ob, b Ob, c Ob]
      :args [f (Hom a b), g (Hom b c)]
      :ret (Hom a c))
    (term ⋅  ; Unicode alias for compose
      :ctx [a Ob, b Ob, c Ob]
      :args [f (→ a b), g (→ b c)]
      :ret (→ a c))"
  [state [_term term-name & {:keys [ctx args ret]}]]
  (when-not (symbol? term-name)
    (throw (ex-info "Term name must be a symbol"
                    {:name term-name})))
  (when-not ret
    (throw (ex-info "Term declaration must have :ret"
                    {:name term-name})))

  ;; Normalize unicode operators to ASCII
  (let [term-name (unicode/normalize-name term-name)
        ;; Parse context bindings
        ctx-bindings (when ctx (partition 2 ctx))
        [state-with-ctx term-ctx] (if ctx-bindings
                                    (parse-context state ctx-bindings)
                                    [state (core/type-ctx)])

        ;; Parse argument bindings in the context (extending it)
        arg-bindings (when args (partition 2 args))
        [state-with-args full-ctx] (if arg-bindings
                                     (parse-context state-with-ctx arg-bindings term-ctx)
                                     [state-with-ctx term-ctx])

        ;; Parse return type
        [state-final ret-type] (parse-type-expr state-with-args ret)

        ;; Create ident for term constructor (in original scope)
        [new-scope-ctx term-ident] (scope/bind (:scope-ctx state) term-name)

        ;; Create the term (term constructor takes full context as args)
        term (core/alg-term term-ident (vec (:idents full-ctx)) ret-type)
        tic (core/term-in-ctx full-ctx term)

        ;; Add to GAT
        new-gat (core/add-term-constructor (:gat state) tic)]

    (-> state
        (assoc :scope-ctx new-scope-ctx)
        (assoc :gat new-gat))))

(defn parse-axiom-decl
  "Parse an axiom (equation) declaration.

  Form:
    (axiom assoc
      :ctx [a Ob, b Ob, c Ob,
            f (Hom a b), g (Hom b c), h (Hom c d)]
      (= lhs rhs))"
  [state [_axiom axiom-name & rest]]
  (when-not (symbol? axiom-name)
    (throw (ex-info "Axiom name must be a symbol"
                    {:name axiom-name})))

  ;; Extract :ctx from rest and find the equation
  (let [rest-map (apply hash-map (take-while #(not (seq? %)) rest))
        ctx (:ctx rest-map)
        eq-form (first (drop-while #(not (seq? %)) rest))]
    (when-not (and (seq? eq-form) (= '= (first eq-form)))
      (throw (ex-info "Axiom must contain an equation with ="
                      {:name axiom-name :form eq-form})))

    (let [[_= lhs-expr rhs-expr] eq-form
          ;; Parse context
          ctx-bindings (when ctx (partition 2 ctx))
          [state-with-ctx axiom-ctx] (if ctx-bindings
                                       (parse-context state ctx-bindings)
                                       [state (core/type-ctx)])

          ;; Parse LHS and RHS (they need a type - we'll use a dummy type for now)
          ;; In a real implementation, we'd infer the type
          dummy-type (core/alg-type (scope/ident (:tag (:gat state)) 999 'DUMMY)
                                    [] core/TYPE)
          [state-lhs lhs] (parse-term-expr state-with-ctx lhs-expr dummy-type)
          [state-rhs rhs] (parse-term-expr state-with-ctx rhs-expr dummy-type)

          ;; Create axiom
          axiom (core/alg-axiom axiom-name axiom-ctx lhs rhs)

          ;; Add to GAT
          new-gat (core/add-axiom (:gat state) axiom)]

      (assoc state :gat new-gat))))

;;; ============================================================================
;;; Theory Macro
;;; ============================================================================

(defn parse-theory-decl
  "Parse a single theory declaration (type, term, or axiom)."
  [state decl]
  (when-not (seq? decl)
    (throw (ex-info "Declaration must be a list"
                    {:decl decl})))

  (let [form (first decl)]
    (case form
      type (parse-type-decl state decl)
      term (parse-term-decl state decl)
      axiom (parse-axiom-decl state decl)
      (throw (ex-info (str "Unknown declaration form: " form)
                      {:form form :decl decl})))))

(defn parse-theory
  "Parse all declarations in a theory."
  [theory-name decls]
  (let [initial-state (fresh-parser-state theory-name)]
    (reduce parse-theory-decl initial-state decls)))

(defn- decl-name
  "Get the name a declaration introduces (type, term, or axiom). Returns nil
   for forms with no introduced name."
  [decl]
  (when (and (seq? decl) (>= (count decl) 2))
    (let [[head nm] decl]
      (when (and (#{'type 'term 'axiom} head) (symbol? nm)) nm))))

(defn expand-using
  "Inline `(using ThParent)` clauses in theory declarations. Each parent's
   stored expanded source-decls are spliced in at the point of the using
   clause. Inheritance is therefore resolved at deftheory-expansion time —
   the resulting decls are scope-flat (all idents live in the child's fresh
   scope) and ready for parse-theory.

   Multiple linear parents work; diamonds throw on the duplicate declared
   name. GATlab.jl's full pushout semantics is a future extension."
  [decls]
  (let [expanded
        (mapcat
         (fn [decl]
           (if (and (seq? decl) (= 'using (first decl)))
             (let [parent-sym (second decl)
                   parent-var (resolve parent-sym)
                   _ (when-not parent-var
                       (throw (ex-info (str "Parent theory not found: " parent-sym
                                            ". Make sure it is defined before its child.")
                                       {:parent parent-sym})))
                   parent-decls (-> @parent-var meta :source-decls)
                   _ (when-not parent-decls
                       (throw (ex-info (str "Parent theory missing :source-decls metadata: " parent-sym
                                            ". Was it defined with the modern `deftheory`?")
                                       {:parent parent-sym})))]
               parent-decls)
             [decl]))
         decls)
        ;; Detect duplicate declared names (diamond inheritance, name collision).
        seen (atom #{})
        _ (doseq [d expanded
                  :let [nm (decl-name d)]
                  :when nm]
            (when (contains? @seen nm)
              (throw (ex-info (str "Duplicate declaration name in inherited theory: " nm
                                   ". Diamond inheritance is not yet supported; "
                                   "factor the shared ancestor into a single `using` chain.")
                              {:name nm})))
            (swap! seen conj nm))]
    (vec expanded)))

(defmacro deftheory
  "Define a Generalized Algebraic Theory.

  Syntax:
    (deftheory TheoryName
      (type TypeName)
      (type TypeName [arg1 Type1, arg2 Type2, ...])
      (term term-name
        :ctx [ctx-var1 Type1, ...]
        :args [arg1 Type1, ...]
        :ret ReturnType)
      (axiom axiom-name
        :ctx [var1 Type1, ...]
        (= lhs rhs)))

  Example:
    (deftheory Category
      (type Ob)
      (type Hom [dom Ob, codom Ob])
      (term compose
        :ctx [a Ob, b Ob, c Ob]
        :args [f (Hom a b), g (Hom b c)]
        :ret (Hom a c))
      (term id
        :ctx [a Ob]
        :ret (Hom a a))
      (axiom assoc
        :ctx [a Ob, b Ob, c Ob, d Ob,
              f (Hom a b), g (Hom b c), h (Hom c d)]
        (= (compose (compose f g) h)
           (compose f (compose g h)))))"
  [theory-name & decls]
  (when-not (symbol? theory-name)
    (throw (ex-info "Theory name must be a symbol"
                    {:name theory-name})))

  (let [;; Resolve (using ThParent) clauses by splicing the parent's stored
        ;; declarations in at expansion time. The expanded list is what we
        ;; parse and what we stash as :source-decls for any descendants.
        expanded-decls (expand-using decls)
        parsed (parse-theory theory-name expanded-decls)
        gat (:gat parsed)
        type-ctors (map (fn [tic] (-> tic :type :head :name)) (:type-constructors gat))
        term-ctors (map (fn [tic] (-> tic :term :head :name)) (:term-constructors gat))
        all-ctors (concat type-ctors term-ctors)

        ;; Helper to compute arity from constructor
        ;; Both TypeInCtx and TermInCtx have a :ctx field
        ctor-arity-fn (fn [ctic]
                        (let [ctx (:ctx ctic)]
                          (if ctx
                            (core/context-length ctx)
                            0)))

        ;; Build ctor -> arity map
        type-tic-map (into {} (map (fn [tic] [(-> tic :type :head :name) tic])
                                   (:type-constructors gat)))
        term-tic-map (into {} (map (fn [tic] [(-> tic :term :head :name) tic])
                                   (:term-constructors gat)))
        ctor-tic-map (merge type-tic-map term-tic-map)

        ;; Generate namespace-unique protocol name to avoid conflicts
        ns-prefix (str/replace (str (ns-name *ns*)) "." "-")
        internal-protocol-name (symbol (str "I" ns-prefix "-" theory-name "Internal"))]

    `(do
       ;; Define the theory with namespace metadata.
       ;; :source-decls holds the *expanded* (post-inheritance) decl list so
       ;; that downstream theories using this one don't have to re-resolve
       ;; the chain.
       (def ~theory-name
         (with-meta
           (:gat (parse-theory '~theory-name '~expanded-decls))
           {:deftheory-ns (quote ~(ns-name *ns*))
            :source-decls (quote ~expanded-decls)}))

       ;; Define internal protocol (all methods take [model args])
       (defprotocol ~internal-protocol-name
         ~(str "Internal protocol for " theory-name " (collection-based)")
         ~@(for [ctor-name all-ctors]
             `(~(symbol (str "-" ctor-name)) [~'model ~'args]
                                             ~(str "Internal method for " ctor-name))))

       ;; Generate wrapper functions with correct arities
       ~@(for [[ctor-name tic] ctor-tic-map]
           (let [arity (ctor-arity-fn tic)
                 arg-syms (vec (repeatedly arity gensym))]
             `(defn ~ctor-name
                ~(str "Wrapper for " ctor-name " constructor from theory " theory-name)
                [~'model ~@arg-syms]
                (~(symbol (str "-" ctor-name)) ~'model ~(vec arg-syms))))))))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(defn format-type
  "Format an AlgType for display."
  [type]
  (if (empty? (:args type))
    (:name (:head type))
    (str "(" (:name (:head type)) " "
         (clojure.string/join " " (map :name (:args type)))
         ")")))

(defn format-context
  "Format a TypeCtx for display."
  [ctx]
  (when (pos? (core/context-length ctx))
    (str "["
         (str/join ", "
                   (map (fn [i t]
                          (str (:name i) " : " (format-type t)))
                        (:idents ctx)
                        (:types ctx)))
         "]")))

(defn format-theory
  "Pretty-print a theory."
  [gat]
  (let [sb (StringBuilder.)]
    (.append sb (str "Theory: " (:name gat) "\n\n"))

    ;; Type constructors
    (when (seq (:type-constructors gat))
      (.append sb "Type Constructors:\n")
      (doseq [tic (:type-constructors gat)]
        (let [ctx (:ctx tic)
              type (:type tic)]
          (.append sb (str "  " (:name (:head type))))
          (when (pos? (core/context-length ctx))
            (.append sb (str " : " (format-context ctx))))
          (.append sb "\n"))))

    ;; Term constructors
    (when (seq (:term-constructors gat))
      (.append sb "\nTerm Constructors:\n")
      (doseq [tic (:term-constructors gat)]
        (let [ctx (:ctx tic)
              term (:term tic)]
          (.append sb (str "  " (:name (:head term))))
          (when (pos? (core/context-length ctx))
            (.append sb (str " : " (format-context ctx))))
          (.append sb (str " → " (format-type (:type term))))
          (.append sb "\n"))))

    ;; Axioms
    (when (seq (:axioms gat))
      (.append sb "\nAxioms:\n")
      (doseq [axiom (:axioms gat)]
        (.append sb (str "  " (:name axiom)))
        (when (pos? (core/context-length (:ctx axiom)))
          (.append sb (str " : " (format-context (:ctx axiom)))))
        (.append sb "\n")))

    (str sb)))
