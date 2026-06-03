(ns katzen.pretty
  "Pretty printing for GATs with unicode support.

  This namespace provides functions to render theories and types
  in a human-readable format, optionally using unicode operators."
  (:require [katzen.core :as core]
            [katzen.unicode :as unicode]
            [katzen.scope :as scope]
            [clojure.string :as str]))

;;; ============================================================================
;;; Type Expression Rendering
;;; ============================================================================

(defn- extract-name
  "Extract the symbol name from an Ident or return the value directly."
  [x]
  (cond
    ;; Ident record has :name field
    (and (map? x) (:name x) (or (symbol? (:name x)) (nil? (:name x))))
    (:name x)

    ;; Already a symbol
    (symbol? x)
    x

    ;; Fallback
    :else
    x))

(defn render-type-expr
  "Render a type expression, optionally with unicode.

  Options:
    :unicode? - Use unicode operators (default: true)"
  [type-expr & {:keys [unicode?] :or {unicode? true}}]
  (let [name-fn (if unicode? unicode/unicode-name identity)]
    (cond
      ;; AlgType with no args - simple type variable
      (and (core/alg-type? type-expr)
           (empty? (:args type-expr)))
      (str (name-fn (extract-name (:head type-expr))))

      ;; AlgType with args - type application: (Head arg1 arg2 ...)
      (and (core/alg-type? type-expr)
           (seq (:args type-expr)))
      (let [head-name (name-fn (extract-name (:head type-expr)))
            args (map #(render-type-expr % :unicode? unicode?) (:args type-expr))]
        (str "(" head-name " " (str/join " " args) ")"))

      ;; Ident by itself (type variable reference)
      (scope/gat-ident? type-expr)
      (str (name-fn (extract-name type-expr)))

      ;; Fallback for unknown structure
      :else
      (str type-expr))))

;;; ============================================================================
;;; Term Rendering
;;; ============================================================================

(defn render-term
  "Render a term constructor, optionally with unicode.

  Options:
    :unicode? - Use unicode operators (default: true)
    :show-type? - Show type signature (default: true)"
  [term-constructor & {:keys [unicode? show-type?] :or {unicode? true show-type? true}}]
  (let [name-fn (if unicode? unicode/unicode-name identity)
        term-name (name-fn (extract-name (-> term-constructor :term :head)))
        term-type (-> term-constructor :term :type)]
    (if show-type?
      (str term-name " : " (render-type-expr term-type :unicode? unicode?))
      (str term-name))))

;;; ============================================================================
;;; Context Rendering
;;; ============================================================================

(defn render-context-binding
  "Render a single context binding.

  Example: a : Ob"
  [ident type-expr & {:keys [unicode?] :or {unicode? true}}]
  (str (extract-name ident) " : " (render-type-expr type-expr :unicode? unicode?)))

;;; ============================================================================
;;; Theory Rendering
;;; ============================================================================

(defn render-type-constructor
  "Render a type constructor declaration."
  [type-constructor & {:keys [unicode?] :or {unicode? true}}]
  (let [name-fn (if unicode? unicode/unicode-name identity)
        type-name (name-fn (extract-name (-> type-constructor :type :head)))
        args (:args type-constructor)]
    (if (seq args)
      (let [arg-strs (map (fn [[ident type-expr]]
                            (render-context-binding ident type-expr :unicode? unicode?))
                          args)]
        (str "  (type " type-name " [" (str/join ", " arg-strs) "])"))
      (str "  (type " type-name ")"))))

(defn render-term-constructor
  "Render a term constructor declaration."
  [term-constructor & {:keys [unicode?] :or {unicode? true}}]
  (let [name-fn (if unicode? unicode/unicode-name identity)
        term-name (name-fn (extract-name (-> term-constructor :term :head)))
        ctx (:ctx term-constructor)
        args (:args term-constructor)
        ret-type (-> term-constructor :term :type)]
    (str/join "\n"
              (concat
               [(str "  (term " term-name)]
               (when (seq ctx)
                 [(str "    :ctx [" (str/join ", "
                                               (map (fn [[ident type-expr]]
                                                      (render-context-binding ident type-expr :unicode? unicode?))
                                                    ctx))
                       "]")])
               (when (seq args)
                 [(str "    :args [" (str/join ", "
                                                (map (fn [[ident type-expr]]
                                                       (render-context-binding ident type-expr :unicode? unicode?))
                                                     args))
                       "]")])
               [(str "    :ret " (render-type-expr ret-type :unicode? unicode?) ")")]))))

(defn render-theory
  "Render a complete theory, optionally with unicode.

  Options:
    :unicode? - Use unicode operators (default: true)
    :show-axioms? - Show axioms (default: false, not yet implemented)"
  [theory & {:keys [unicode? show-axioms?] :or {unicode? true show-axioms? false}}]
  (let [name (:name theory)
        type-constructors (:type-constructors theory)
        term-constructors (:term-constructors theory)]
    (str/join "\n"
              (concat
               [(str "(theory/deftheory " name)]
               (map #(render-type-constructor % :unicode? unicode?) type-constructors)
               [""]  ; Blank line between types and terms
               (map #(render-term-constructor % :unicode? unicode?) term-constructors)
               [")"]))))

;;; ============================================================================
;;; REPL Display
;;; ============================================================================

(defn theory-summary
  "Print a concise summary of a theory.

  Shows theory name, number of types/terms, and optionally lists them.

  Options:
    :unicode? - Use unicode for names (default: true)
    :show-names? - List type and term names (default: true)"
  [theory & {:keys [unicode? show-names?] :or {unicode? true show-names? true}}]
  (let [name-fn (if unicode? unicode/unicode-name identity)
        type-names (map #(name-fn (extract-name (-> % :type :head))) (:type-constructors theory))
        term-names (map #(name-fn (extract-name (-> % :term :head))) (:term-constructors theory))]
    (str/join "\n"
              (concat
               [(str "Theory: " (:name theory))]
               [(str "  Types: " (count type-names))]
               (when show-names?
                 [(str "    " (str/join ", " type-names))])
               [(str "  Terms: " (count term-names))]
               (when show-names?
                 [(str "    " (str/join ", " term-names))])))))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defn pprint-theory
  "Pretty print a theory with unicode (default) or ASCII.

  Examples:
    (pprint-theory ThCategory)              ; Unicode
    (pprint-theory ThCategory :unicode? false)  ; ASCII"
  [theory & opts]
  (println (apply render-theory theory opts)))

(defn summary
  "Print a concise summary of a theory.

  Examples:
    (summary ThCategory)
    (summary ThCategory :unicode? false)"
  [theory & opts]
  (println (apply theory-summary theory opts)))
