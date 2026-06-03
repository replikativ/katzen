(ns katzen.expr-interop
  "Expression interoperability - convert between GAT structures and s-expressions.

  This module provides toexpr/fromexpr functions that convert between
  our internal GAT structures (AlgType, AlgTerm, etc.) and Clojure s-expressions.

  This is essential for:
  - Debugging (human-readable output)
  - Macros (parsing user input)
  - Serialization (saving/loading theories)
  - Multi-level context disambiguation (x vs x!1)"
  (:require [katzen.core :as core]
            [katzen.scope :as scope]
            [clojure.string :as str]))

;;; ============================================================================
;;; Scope Context for Disambiguation
;;; ============================================================================

(defrecord ScopeLevel [tag level])

(defn scope-level
  "Create a scope level for disambiguation."
  [tag level]
  (->ScopeLevel tag level))

(defrecord ScopeList [levels]
  Object
  (toString [_]
    (str "ScopeList(" (count levels) " levels)")))

(defn scope-list
  "Create a multi-level scope for disambiguation.

  When converting to expressions, idents from different scope levels
  with the same name will be disambiguated with suffixes like x!1, x!2, etc."
  [scopes]
  (->ScopeList (vec (map-indexed (fn [idx sc] (scope-level (:tag sc) (inc idx))) scopes))))

(defn scope-list?
  "Check if value is a ScopeList."
  [x]
  (instance? ScopeList x))

(defn find-scope-level
  "Find the level of a tag in a ScopeList.

  Returns the level (1-indexed) if found, nil otherwise."
  [slist tag]
  (when (scope-list? slist)
    (some (fn [sl]
            (when (= tag (:tag sl))
              (:level sl)))
          (:levels slist))))

;;; ============================================================================
;;; toexpr - Convert GAT structures to s-expressions
;;; ============================================================================

(defn ident-toexpr
  "Convert an Ident to a symbol, with optional disambiguation.

  If scope-ctx is a ScopeList with multiple levels, and the ident's tag
  is not in the most recent level, append !N suffix for disambiguation.

  Anonymous idents (nil name) are converted to var\"#LID\" or var\"#LID!N\"."
  ([ident]
   (ident-toexpr nil ident))
  ([scope-ctx ident]
   (let [name (:name ident)
         tag (:tag ident)
         lid (:lid ident)]
     (cond
       ;; Anonymous ident
       (nil? name)
       (if-let [level (find-scope-level scope-ctx tag)]
         (if (= 1 level)
           (symbol (str "var\"#" lid "\""))
           (symbol (str "var\"#" lid "!" level "\"")))
         (symbol (str "var\"#" lid "\"")))

       ;; Named ident with scope disambiguation
       (and (scope-list? scope-ctx) (find-scope-level scope-ctx tag))
       (let [level (find-scope-level scope-ctx tag)
             most-recent? (= level 1)]
         (if most-recent?
           name
           (symbol (str (clojure.core/name name) "!" level))))

       ;; Simple named ident
       :else
       name))))

(defn type-toexpr
  "Convert an AlgType to an s-expression.

  Examples:
    Ob => 'Ob
    (Hom a b) => '(Hom a b)"
  ([type]
   (type-toexpr nil type))
  ([scope-ctx type]
   (if (core/alg-type? type)
     (let [head (ident-toexpr scope-ctx (:head type))
           args (map #(if (scope/gat-ident? %)
                        (ident-toexpr scope-ctx %)
                        (type-toexpr scope-ctx %))
                     (:args type))]
       (if (empty? args)
         head
         (cons head args)))
     (throw (ex-info "Not an AlgType" {:type type})))))

(defn term-toexpr
  "Convert an AlgTerm to an s-expression.

  Examples:
    (id a) => '(id a)
    (compose f g) => '(compose f g)
    (compose (compose f g) h) => '(compose (compose f g) h)"
  ([term]
   (term-toexpr nil term))
  ([scope-ctx term]
   (if (core/alg-term? term)
     (let [head (ident-toexpr scope-ctx (:head term))
           args (map #(cond
                        (scope/gat-ident? %)
                        (ident-toexpr scope-ctx %)

                        (core/alg-term? %)
                        (term-toexpr scope-ctx %)

                        :else
                        (throw (ex-info "Invalid term argument" {:arg %})))
                     (:args term))]
       (if (empty? args)
         head
         (cons head args)))
     (throw (ex-info "Not an AlgTerm" {:term term})))))

(defn toexpr
  "Convert a GAT structure to an s-expression.

  Accepts:
  - Ident => symbol (with optional disambiguation)
  - AlgType => (Head arg1 arg2 ...)
  - AlgTerm => (head arg1 arg2 ...)
  - AlgSort => symbol

  Optional scope-ctx for disambiguation:
  - nil or ScopeContext => no disambiguation
  - ScopeList => disambiguate with !N suffixes"
  ([x]
   (toexpr nil x))
  ([scope-ctx x]
   (cond
     (scope/gat-ident? x) (ident-toexpr scope-ctx x)
     (core/alg-type? x) (type-toexpr scope-ctx x)
     (core/alg-term? x) (term-toexpr scope-ctx x)
     (core/alg-sort? x) (:name x)
     :else (throw (ex-info "Don't know how to convert to expression" {:value x})))))

;;; ============================================================================
;;; fromexpr - Convert s-expressions to GAT structures
;;; ============================================================================

(defn parse-disambiguated-symbol
  "Parse a symbol with optional !N suffix.

  Returns [name level] where level is 1 if no suffix."
  [sym]
  (let [s (clojure.core/name sym)
        parts (str/split s #"!")]
    (if (= 1 (count parts))
      [sym 1]
      [(symbol (first parts)) (Integer/parseInt (second parts))])))

(defn parse-anonymous-symbol
  "Parse an anonymous variable symbol like var\"#1\" or var\"#1!2\".

  Returns [lid level]."
  [sym]
  (let [s (clojure.core/name sym)]
    (when (and (str/starts-with? s "var\"#")
               (str/ends-with? s "\""))
      (let [inner (subs s 5 (dec (count s)))  ; Remove var"# and trailing "
            parts (str/split inner #"!")]
        (if (= 1 (count parts))
          [(Integer/parseInt (first parts)) 1]
          [(Integer/parseInt (first parts)) (Integer/parseInt (second parts))])))))

(defn find-ident-in-scope
  "Find an ident in a scope context by name and level.

  For ScopeList, level determines which scope to search (1 = most recent)."
  [scope-ctx name level]
  (cond
    ;; ScopeContext - simple lookup
    (instance? katzen.scope.ScopeContext scope-ctx)
    (when (= 1 level)
      (scope/lookup scope-ctx name))

    ;; ScopeList - need to find the right level
    (scope-list? scope-ctx)
    (throw (ex-info "ScopeList lookup not yet implemented" {:name name :level level}))

    :else
    (throw (ex-info "Unknown scope context type" {:scope-ctx scope-ctx}))))

(defn fromexpr-ident
  "Convert a symbol to an Ident using scope context.

  Handles:
  - Simple names: x => lookup in scope
  - Disambiguated: x!2 => lookup in level 2
  - Anonymous: var\"#1\" => create anonymous ident"
  [scope-ctx expr]
  (when-not (symbol? expr)
    (throw (ex-info "Ident expression must be a symbol" {:expr expr})))

  (if-let [[lid level] (parse-anonymous-symbol expr)]
    ;; Anonymous ident
    (throw (ex-info "Anonymous ident creation not yet implemented" {:lid lid :level level}))

    ;; Named ident
    (let [[name level] (parse-disambiguated-symbol expr)]
      (or (find-ident-in-scope scope-ctx name level)
          (throw (ex-info (str "Ident not found in scope: " name)
                          {:name name :level level}))))))

(defn fromexpr-type
  "Convert an s-expression to an AlgType using scope context."
  [scope-ctx expr]
  (cond
    ;; Simple type: Ob
    (symbol? expr)
    (let [head-ident (fromexpr-ident scope-ctx expr)]
      (core/alg-type head-ident [] core/TYPE))

    ;; Type with args: (Hom a b)
    (seq? expr)
    (let [[head & args] expr
          head-ident (fromexpr-ident scope-ctx head)
          arg-idents (mapv #(fromexpr-ident scope-ctx %) args)]
      (core/alg-type head-ident arg-idents core/TYPE))

    :else
    (throw (ex-info "Invalid type expression" {:expr expr}))))

(defn fromexpr-term
  "Convert an s-expression to an AlgTerm using scope context."
  [scope-ctx expr expected-type]
  (cond
    ;; Simple term: f
    (symbol? expr)
    (let [ident (fromexpr-ident scope-ctx expr)]
      (core/alg-term ident [] expected-type))

    ;; Term with args: (compose f g)
    (seq? expr)
    (let [[head & args] expr
          head-ident (fromexpr-ident scope-ctx head)
          ;; Args can be idents or nested terms
          arg-values (mapv (fn [arg]
                             (if (seq? arg)
                               (fromexpr-term scope-ctx arg expected-type)
                               (fromexpr-ident scope-ctx arg)))
                           args)]
      (core/alg-term head-ident arg-values expected-type))

    :else
    (throw (ex-info "Invalid term expression" {:expr expr}))))

(defn fromexpr
  "Convert an s-expression to a GAT structure using scope context.

  Type is one of: Ident, AlgType, AlgTerm, AlgSort

  For AlgTerm, you can optionally provide expected-type."
  ([scope-ctx expr target-type]
   (fromexpr scope-ctx expr target-type nil))
  ([scope-ctx expr target-type expected-type]
   (case target-type
     Ident (fromexpr-ident scope-ctx expr)
     AlgType (fromexpr-type scope-ctx expr)
     AlgTerm (fromexpr-term scope-ctx expr expected-type)
     AlgSort (core/alg-sort expr)
     (throw (ex-info "Unknown target type" {:target-type target-type})))))
