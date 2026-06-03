(ns katzen.model
  "Model compilation infrastructure for GATs using protocols.

  Models provide concrete or symbolic implementations of theories:
  - Concrete models: Implement term constructors as computations
  - Symbolic models: Build ASTs that can be rewritten/normalized

  Key design: Each theory generates a protocol with methods for all constructors.
  Models extend this protocol with their implementations."
  (:require [katzen.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]))

;;; ============================================================================
;;; Model Protocol (meta-protocol for all models)
;;; ============================================================================

(defprotocol IModel
  "Protocol for models that implement theories."
  (theory [model]
    "Get the GAT this model implements.")
  (type-mapping [model]
    "Get the map from theory sorts to Clojure types.")
  (model-type [model]
    "Get the type of this model (:concrete, :symbolic, :migrated)."))

;;; ============================================================================
;;; Theory Introspection
;;; ============================================================================

(defn type-constructor-names
  "Extract names of all type constructors from a theory."
  [theory]
  (mapv (fn [tic]
          (-> tic :type :head :name))
        (:type-constructors theory)))

(defn term-constructor-names
  "Extract names of all term constructors from a theory."
  [theory]
  (mapv (fn [tic]
          (-> tic :term :head :name))
        (:term-constructors theory)))

(defn constructor-context
  "Get the context (parameters) for a constructor."
  [constructor-in-ctx]
  (:ctx constructor-in-ctx))

(defn constructor-arity
  "Get the arity (number of explicit parameters) for a constructor.

  For type constructors: arity = context length (parameters ARE the context)
  For term constructors: arity = full context length (includes implicit ctx + explicit args)

  Note: In the theory, term constructors have :ctx (implicit type params) and :args (explicit params).
  During parsing, these are combined into a single context in TermInCtx.
  We return the full length because that's what we need for wrapper generation."
  [constructor-in-ctx]
  (core/context-length (:ctx constructor-in-ctx)))

;;; ============================================================================
;;; definstance Helper Functions
;;; ============================================================================

(defn validate-instance-methods
  "Validate that all required methods are implemented."
  [theory implemented-methods]
  (let [required-type-ctors (set (type-constructor-names theory))
        required-term-ctors (set (term-constructor-names theory))
        required (set/union required-type-ctors required-term-ctors)
        implemented (set (map first implemented-methods))
        missing (set/difference required implemented)]
    {:valid? (empty? missing)
     :missing (vec missing)
     :required required
     :implemented implemented}))

(defn parse-method-impl
  "Parse a method implementation from definstance body."
  [form]
  (when (and (seq? form) (>= (count form) 2))
    (let [[method-name arg-vec & body] form]
      (when (and (symbol? method-name)
                 (vector? arg-vec)
                 (>= (count arg-vec) 1))
        [method-name arg-vec body]))))

(defn extract-method-impls
  "Extract all method implementations from definstance body."
  [body]
  (->> body
       (filter seq?)
       (map parse-method-impl)
       (filter some?)
       vec))

;;; ============================================================================
;;; definstance Macro
;;; ============================================================================

(defmacro definstance
  "Define a concrete model that implements a theory via protocol extension.

  Example:
    (definstance FinSetCategory ThCategory
      {:ob-type :integer
       :hom-type :vector}

      (Ob [model args]
        (let [[n] args]
          (pos? n)))

      (compose [model args]
        (let [[f g] args]
          (mapv #(nth g (dec %)) f))))

  Note: Protocol methods receive args as a collection.
  Use destructuring in the method body to extract individual arguments.

  This generates:
  1. Protocol for the theory (all methods take [model args])
  2. Wrapper functions with proper arities for nice calling convention
  3. A record implementing IModel
  4. Protocol extension with method implementations (via extend-protocol)
  5. Registration in the model registry"
  [model-name theory-sym type-map & method-impls]
  (let [theory-var (resolve theory-sym)]

    ;; Validation at macro expansion time
    (when-not theory-var
      (throw (ex-info (str "Theory not found: " theory-sym)
                      {:theory theory-sym})))

    (let [theory @theory-var
          methods (extract-method-impls method-impls)
          validation (validate-instance-methods theory methods)

          ;; Get the theory's deftheory namespace from metadata
          theory-deftheory-ns (-> theory meta :deftheory-ns)
          _ (when-not theory-deftheory-ns
              (throw (ex-info (str "Theory missing :deftheory-ns metadata. "
                                   "Was it defined with deftheory?")
                              {:theory (:name theory)})))

          ;; Construct namespace-prefixed protocol name to match deftheory's generation
          ns-prefix (clojure.string/replace (str theory-deftheory-ns) "." "-")
          internal-protocol-name (symbol (str "I" ns-prefix "-" (:name theory) "Internal"))
          qualified-protocol-name (symbol (str theory-deftheory-ns) (str internal-protocol-name))

          record-name (symbol (str model-name "Record"))

          ;; Get constructor info with arities
          type-tic-map (into {} (map (fn [tic] [(-> tic :type :head :name) tic])
                                     (:type-constructors theory)))
          term-tic-map (into {} (map (fn [tic] [(-> tic :term :head :name) tic])
                                     (:term-constructors theory)))
          ctor-tic-map (merge type-tic-map term-tic-map)]

      (when-not (:valid? validation)
        (throw (ex-info (str "Missing required methods: " (:missing validation))
                        {:model model-name
                         :theory (:name theory)
                         :missing (:missing validation)
                         :required (:required validation)
                         :implemented (:implemented validation)})))

      `(do
         ;; Protocol and wrappers are now generated by deftheory in the theory namespace
         ;; We just need to resolve the internal protocol that should already exist
         (let [resolved-protocol# (resolve '~qualified-protocol-name)]
           (when-not resolved-protocol#
             (throw (ex-info (str "Protocol not found: " '~qualified-protocol-name
                                 ". Did you define the theory with deftheory?")
                            {:protocol '~qualified-protocol-name
                             :theory '~theory-sym}))))

         ;; Define the model record (without protocol extension)
         (defrecord ~record-name [~'theory-ref ~'type-map ~'model-type]
           IModel
           (~'theory [~'_] ~'theory-ref)
           (~'type-mapping [~'_] ~'type-map)
           (~'model-type [~'_] ~'model-type))

         ;; Extend the internal protocol AFTER record exists
         (extend-protocol ~qualified-protocol-name
           ~record-name
           ~@(for [[method-name arg-vec body] methods]
               (list (symbol (str "-" method-name))
                     arg-vec
                     (cons 'do body))))

         ;; Create constructor function
         (defn ~(symbol (str "->" model-name))
           ~(str "Create a " model-name " model instance.")
           []
           (~(symbol (str "->" record-name)) ~theory-sym ~type-map :concrete))

         '~model-name))))

;;; ============================================================================
;;; Symbolic Model Infrastructure
;;; ============================================================================

(defrecord SymbolicExpr [head args type]
  Object
  (toString [_]
    (if (empty? args)
      (str head)
      (str "(" head " " (str/join " " args) ")"))))

(defn symbolic-expr
  "Create a symbolic expression (like AlgTerm but for runtime values)."
  [head args type]
  (->SymbolicExpr head args type))

(defn symbolic-expr?
  "Check if value is a symbolic expression."
  [x]
  (instance? SymbolicExpr x))

;;; ============================================================================
;;; defsymbolic Macro
;;; ============================================================================

(defmacro defsymbolic
  "Define a symbolic model that builds ASTs instead of computing.

  Example:
    (defsymbolic SymCategory ThCategory
      {:normalize? true
       :rules ThCategory-rules})

  This creates methods that build SymbolicExpr trees.
  Uses the same collection-based protocol as definstance."
  [model-name theory-sym options]
  (let [theory-var (resolve theory-sym)]

    (when-not theory-var
      (throw (ex-info (str "Theory not found: " theory-sym)
                      {:theory theory-sym})))

    (let [theory @theory-var
          type-ctors (type-constructor-names theory)
          term-ctors (term-constructor-names theory)

          ;; Get the theory's deftheory namespace from metadata (same as definstance)
          theory-deftheory-ns (-> theory meta :deftheory-ns)
          _ (when-not theory-deftheory-ns
              (throw (ex-info (str "Theory missing :deftheory-ns metadata. "
                                   "Was it defined with deftheory?")
                              {:theory (:name theory)})))

          ;; Construct namespace-prefixed protocol name to match deftheory's generation
          ns-prefix (clojure.string/replace (str theory-deftheory-ns) "." "-")
          internal-protocol-name (symbol (str "I" ns-prefix "-" (:name theory) "Internal"))
          qualified-protocol-name (symbol (str theory-deftheory-ns) (str internal-protocol-name))

          record-name (symbol (str model-name "Record"))
          normalize? (:normalize? options false)
          rules (:rules options nil)]

      `(do
         ;; Protocol and wrappers are now generated by deftheory in the theory namespace
         ;; We just need to resolve the internal protocol that should already exist
         (let [resolved-protocol# (resolve '~qualified-protocol-name)]
           (when-not resolved-protocol#
             (throw (ex-info (str "Protocol not found: " '~qualified-protocol-name
                                 ". Did you define the theory with deftheory?")
                            {:protocol '~qualified-protocol-name
                             :theory '~theory-sym}))))

         ;; Define the model record (without protocol extension)
         (defrecord ~record-name [~'theory-ref ~'options ~'model-type]
           IModel
           (~'theory [~'_] ~'theory-ref)
           (~'type-mapping [~'_] {}) ; Symbolic models don't have type mappings
           (~'model-type [~'_] ~'model-type))

         ;; Extend the internal protocol AFTER record exists
         (extend-protocol ~qualified-protocol-name
           ~record-name

           ;; Type constructor implementations
           ~@(for [ctor type-ctors]
               (list (symbol (str "-" ctor))
                     ['_ 'args]
                     `(symbolic-expr '~ctor ~'args :type)))

           ;; Term constructor implementations
           ~@(for [ctor term-ctors]
               (list (symbol (str "-" ctor))
                     ['_ 'args]
                     `(let [expr# (symbolic-expr '~ctor ~'args :term)]
                        (if (and ~normalize? ~rules)
                          ;; TODO: Apply normalization
                          expr#
                          expr#)))))

         ;; Create constructor function
         (defn ~(symbol (str "->" model-name))
           ~(str "Create a " model-name " symbolic model instance.")
           []
           (~(symbol (str "->" record-name)) ~theory-sym ~options :symbolic))

         '~model-name))))

;;; ============================================================================
;;; Model Utilities
;;; ============================================================================

(defn concrete-model?
  "Check if a model is a concrete model."
  [model]
  (and (satisfies? IModel model)
       (= :concrete (model-type model))))

(defn symbolic-model?
  "Check if a model is a symbolic model."
  [model]
  (and (satisfies? IModel model)
       (= :symbolic (model-type model))))

