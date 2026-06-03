(ns katzen.core
  "Core data structures for Generalized Algebraic Theories (GATs).

  This implements the core AST types from GATlab.jl:
  - AlgSort: Sorts in the algebraic theory (like TYPE)
  - AlgType: Type expressions (nullary constructors, type constructors)
  - AlgTerm: Term expressions (constants, operations)
  - AlgAxiom: Equations between terms

  These form the basis for defining mathematical theories with dependent types
  and equational axioms."
  (:require [katzen.scope :as scope]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;;; ============================================================================
;;; Algebraic Sorts
;;; ============================================================================

(defrecord AlgSort [name]
  Object
  (toString [_] (str "AlgSort(" name ")")))

(defn alg-sort
  "Create an algebraic sort (like TYPE in GATlab).

  Sorts are the 'types of types' - they classify what kind of thing a type is."
  [name]
  {:pre [(symbol? name)]}
  (->AlgSort name))

(def TYPE
  "The standard sort for types."
  (alg-sort 'TYPE))

(defn alg-sort?
  "Check if value is an AlgSort."
  [x]
  (instance? AlgSort x))

;;; ============================================================================
;;; Algebraic Types
;;; ============================================================================

(defrecord AlgType [head args sort]
  Object
  (toString [_]
    (if (empty? args)
      (str head)
      (str "(" head " " (str/join " " args) ")"))))

(defn alg-type
  "Create an algebraic type expression.

  Parameters:
    head - Ident for the type constructor
    args - Vector of argument terms
    sort - AlgSort this type belongs to (usually TYPE)"
  [head args sort]
  {:pre [(scope/gat-ident? head)
         (vector? args)
         (alg-sort? sort)]}
  (->AlgType head args sort))

(defn alg-type?
  "Check if value is an AlgType."
  [x]
  (instance? AlgType x))

;;; ============================================================================
;;; Algebraic Terms
;;; ============================================================================

(defrecord AlgTerm [head args type]
  Object
  (toString [_]
    (if (empty? args)
      (str head)
      (str "(" head " " (str/join " " args) ")"))))

(defn alg-term
  "Create an algebraic term expression.

  Parameters:
    head - Ident for the term constructor
    args - Vector of argument terms
    type - AlgType this term has"
  [head args type]
  {:pre [(scope/gat-ident? head)
         (vector? args)
         (alg-type? type)]}
  (->AlgTerm head args type))

(defn alg-term?
  "Check if value is an AlgTerm."
  [x]
  (instance? AlgTerm x))

;;; ============================================================================
;;; Type and Term Contexts
;;; ============================================================================

(defrecord TypeCtx [idents types]
  Object
  (toString [_]
    (str "TypeCtx(" (count idents) " bindings)")))

(defn type-ctx
  "Create a type context from parallel vectors of idents and types.

  A type context is like Γ in type theory: a list of (x : T) bindings."
  ([]
   (->TypeCtx [] []))
  ([idents types]
   {:pre [(= (count idents) (count types))
          (every? scope/gat-ident? idents)
          (every? alg-type? types)]}
   (->TypeCtx idents types)))

(defn type-ctx?
  "Check if value is a TypeCtx."
  [x]
  (instance? TypeCtx x))

(defn add-binding
  "Add a new (ident : type) binding to the context."
  [ctx ident type]
  {:pre [(type-ctx? ctx)
         (scope/gat-ident? ident)
         (alg-type? type)]}
  (-> ctx
      (update :idents conj ident)
      (update :types conj type)))

(defn add-bindings
  "Add multiple bindings to the context."
  [ctx idents types]
  (reduce
   (fn [c [i t]] (add-binding c i t))
   ctx
   (map vector idents types)))

(defn lookup-type
  "Look up the type of an ident in the context."
  [ctx ident]
  (when-let [idx (.indexOf ^clojure.lang.PersistentVector (:idents ctx) ident)]
    (when (>= idx 0)
      (nth (:types ctx) idx))))

(defn has-binding-for?
  "Check if context has a binding for the given ident."
  [ctx ident]
  (some? (lookup-type ctx ident)))

(defn context-length
  "Get the number of bindings in the context."
  [ctx]
  (count (:idents ctx)))

;;; ============================================================================
;;; Type and Term in Context
;;; ============================================================================

(defrecord TypeInCtx [ctx type]
  Object
  (toString [_]
    (str "TypeInCtx[" (context-length ctx) "](" type ")")))

(defn type-in-ctx
  "Create a type in a context: Γ ⊢ T.

  Represents a type expression that may refer to variables bound in the context."
  [ctx type]
  {:pre [(type-ctx? ctx)
         (alg-type? type)]}
  (->TypeInCtx ctx type))

(defn type-in-ctx?
  "Check if value is a TypeInCtx."
  [x]
  (instance? TypeInCtx x))

(defrecord TermInCtx [ctx term]
  Object
  (toString [_]
    (str "TermInCtx[" (context-length ctx) "](" term ")")))

(defn term-in-ctx
  "Create a term in a context: Γ ⊢ t : T.

  Represents a term expression that may refer to variables bound in the context."
  [ctx term]
  {:pre [(type-ctx? ctx)
         (alg-term? term)]}
  (->TermInCtx ctx term))

(defn term-in-ctx?
  "Check if value is a TermInCtx."
  [x]
  (instance? TermInCtx x))

;;; ============================================================================
;;; Axioms (Equations)
;;; ============================================================================

(defrecord AlgAxiom [name ctx lhs rhs]
  Object
  (toString [_]
    (str "AlgAxiom(" name "): " lhs " = " rhs)))

(defn alg-axiom
  "Create an equation axiom: Γ ⊢ lhs = rhs.

  Parameters:
    name - Symbol naming this axiom
    ctx - TypeCtx with variables the equation ranges over
    lhs - Left-hand side AlgTerm
    rhs - Right-hand side AlgTerm"
  [name ctx lhs rhs]
  {:pre [(symbol? name)
         (type-ctx? ctx)
         (alg-term? lhs)
         (alg-term? rhs)]}
  (->AlgAxiom name ctx lhs rhs))

(defn alg-axiom?
  "Check if value is an AlgAxiom."
  [x]
  (instance? AlgAxiom x))

;;; ============================================================================
;;; GAT (Generalized Algebraic Theory)
;;; ============================================================================

(defrecord GAT [name tag sorts type-constructors term-constructors axioms]
  Object
  (toString [_]
    (str "GAT(" name "): "
         (count type-constructors) " types, "
         (count term-constructors) " terms, "
         (count axioms) " axioms")))

(defn gat
  "Create a Generalized Algebraic Theory.

  Parameters:
    name - Symbol naming this theory
    tag - ScopeTag for this theory's scope
    sorts - Vector of AlgSorts
    type-constructors - Vector of TypeInCtx (type declarations)
    term-constructors - Vector of TermInCtx (term declarations)
    axioms - Vector of AlgAxioms (equations)"
  [name tag sorts type-constructors term-constructors axioms]
  {:pre [(symbol? name)
         (scope/scope-tag? tag)
         (vector? sorts)
         (vector? type-constructors)
         (vector? term-constructors)
         (vector? axioms)]}
  (->GAT name tag sorts type-constructors term-constructors axioms))

(defn gat?
  "Check if value is a GAT."
  [x]
  (instance? GAT x))

(defn empty-gat
  "Create an empty GAT with the given name."
  [name]
  (gat name (scope/scope-tag) [TYPE] [] [] []))

;;; ============================================================================
;;; IScoped Implementation for Core Types
;;; ============================================================================

(extend-protocol scope/IScoped
  AlgSort
  (retag [this _] this)
  (rename [this _ _] this)
  (reident [this _] this)

  AlgType
  (retag [this tag-map]
    (-> this
        (update :head scope/retag tag-map)
        (update :args scope/retag tag-map)))

  (rename [this tag name-map]
    (-> this
        (update :head scope/rename tag name-map)
        (update :args scope/rename tag name-map)))

  (reident [this ident-map]
    (-> this
        (update :head scope/reident ident-map)
        (update :args scope/reident ident-map)))

  AlgTerm
  (retag [this tag-map]
    (-> this
        (update :head scope/retag tag-map)
        (update :args scope/retag tag-map)
        (update :type scope/retag tag-map)))

  (rename [this tag name-map]
    (-> this
        (update :head scope/rename tag name-map)
        (update :args scope/rename tag name-map)
        (update :type scope/rename tag name-map)))

  (reident [this ident-map]
    (-> this
        (update :head scope/reident ident-map)
        (update :args scope/reident ident-map)
        (update :type scope/reident ident-map)))

  TypeCtx
  (retag [this tag-map]
    (-> this
        (update :idents scope/retag tag-map)
        (update :types scope/retag tag-map)))

  (rename [this tag name-map]
    (-> this
        (update :idents scope/rename tag name-map)
        (update :types scope/rename tag name-map)))

  (reident [this ident-map]
    (-> this
        (update :idents scope/reident ident-map)
        (update :types scope/reident ident-map)))

  TypeInCtx
  (retag [this tag-map]
    (-> this
        (update :ctx scope/retag tag-map)
        (update :type scope/retag tag-map)))

  (rename [this tag name-map]
    (-> this
        (update :ctx scope/rename tag name-map)
        (update :type scope/rename tag name-map)))

  (reident [this ident-map]
    (-> this
        (update :ctx scope/reident ident-map)
        (update :type scope/reident ident-map)))

  TermInCtx
  (retag [this tag-map]
    (-> this
        (update :ctx scope/retag tag-map)
        (update :term scope/retag tag-map)))

  (rename [this tag name-map]
    (-> this
        (update :ctx scope/rename tag name-map)
        (update :term scope/rename tag name-map)))

  (reident [this ident-map]
    (-> this
        (update :ctx scope/reident ident-map)
        (update :term scope/reident ident-map)))

  AlgAxiom
  (retag [this tag-map]
    (-> this
        (update :ctx scope/retag tag-map)
        (update :lhs scope/retag tag-map)
        (update :rhs scope/retag tag-map)))

  (rename [this tag name-map]
    (-> this
        (update :ctx scope/rename tag name-map)
        (update :lhs scope/rename tag name-map)
        (update :rhs scope/rename tag name-map)))

  (reident [this ident-map]
    (-> this
        (update :ctx scope/reident ident-map)
        (update :lhs scope/reident ident-map)
        (update :rhs scope/reident ident-map)))

  GAT
  (retag [this tag-map]
    (-> this
        (update :tag #(get tag-map % %))
        (update :type-constructors scope/retag tag-map)
        (update :term-constructors scope/retag tag-map)
        (update :axioms scope/retag tag-map)))

  (rename [this tag name-map]
    (if (= tag (:tag this))
      (-> this
          (update :type-constructors scope/rename tag name-map)
          (update :term-constructors scope/rename tag name-map)
          (update :axioms scope/rename tag name-map))
      this))

  (reident [this ident-map]
    (-> this
        (update :type-constructors scope/reident ident-map)
        (update :term-constructors scope/reident ident-map)
        (update :axioms scope/reident ident-map))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn get-type-constructor
  "Get a type constructor from the GAT by name."
  [gat name]
  {:pre [(gat? gat)]}
  (->> (:type-constructors gat)
       (filter #(= name (-> % :type :head :name)))
       first))

(defn get-term-constructor
  "Get a term constructor from the GAT by name."
  [gat name]
  {:pre [(gat? gat)]}
  (->> (:term-constructors gat)
       (filter #(= name (-> % :term :head :name)))
       first))

(defn get-axiom
  "Get an axiom from the GAT by name."
  [gat name]
  {:pre [(gat? gat)]}
  (->> (:axioms gat)
       (filter #(= name (:name %)))
       first))

(defn add-type-constructor
  "Add a type constructor to the GAT."
  [gat type-in-ctx]
  {:pre [(gat? gat)
         (type-in-ctx? type-in-ctx)]}
  (update gat :type-constructors conj type-in-ctx))

(defn add-term-constructor
  "Add a term constructor to the GAT."
  [gat term-in-ctx]
  {:pre [(gat? gat)
         (term-in-ctx? term-in-ctx)]}
  (update gat :term-constructors conj term-in-ctx))

(defn add-axiom
  "Add an axiom to the GAT."
  [gat axiom]
  {:pre [(gat? gat)
         (alg-axiom? axiom)]}
  (update gat :axioms conj axiom))

;;; ============================================================================
;;; Specs
;;; ============================================================================

(s/def ::alg-sort alg-sort?)
(s/def ::alg-type alg-type?)
(s/def ::alg-term alg-term?)
(s/def ::type-ctx type-ctx?)
(s/def ::type-in-ctx type-in-ctx?)
(s/def ::term-in-ctx term-in-ctx?)
(s/def ::alg-axiom alg-axiom?)
(s/def ::gat gat?)
