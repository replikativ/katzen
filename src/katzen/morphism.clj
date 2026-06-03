(ns katzen.morphism
  "Theory morphisms for GATs.

  A theory morphism is a structure-preserving map between theories that
  translates types and terms from one theory (domain) to another (codomain).

  Example:
    (defmorphism OpCategory [Category => Category]
      (Ob => Ob)
      (Hom [a Ob, b Ob] => (Hom b a))
      (compose [a Ob, b Ob, c Ob, f (Hom a b), g (Hom b c)]
        => (compose g f))
      (id [a Ob] => (id a)))"
  (:require [katzen.core :as core]
            [katzen.scope :as scope]
            [clojure.set :as set]))

;;; ============================================================================
;;; Theory Morphism Data Structures
;;; ============================================================================

(defrecord TheoryMorphism [name dom codom type-map term-map]
  Object
  (toString [_]
    (str "TheoryMorphism(" name ": " (:name dom) " => " (:name codom) ")")))

(defn theory-morphism
  "Create a theory morphism.

  Parameters:
    name - Symbol naming this morphism
    dom - Domain GAT
    codom - Codomain GAT
    type-map - Map from domain type idents to TypeInCtx in codomain
    term-map - Map from domain term idents to TermInCtx in codomain"
  [name dom codom type-map term-map]
  {:pre [(symbol? name)
         (core/gat? dom)
         (core/gat? codom)
         (map? type-map)
         (map? term-map)]}
  (->TheoryMorphism name dom codom type-map term-map))

(defn theory-morphism?
  "Check if value is a TheoryMorphism."
  [x]
  (instance? TheoryMorphism x))

;;; ============================================================================
;;; Substitution
;;; ============================================================================

(defn substitute-ident
  "Substitute an ident using an ident map.

  If the ident is in the map, return its replacement (which can be an Ident
  or an AlgTerm/AlgType). Otherwise return the ident unchanged."
  [ident-map ident]
  (get ident-map ident ident))

(defn substitute-in-type
  "Recursively substitute idents in an AlgType using an ident map."
  [ident-map type]
  (if (core/alg-type? type)
    (let [head-result (substitute-ident ident-map (:head type))]
      (cond
        ;; If head maps to another type, return that type
        (core/alg-type? head-result)
        head-result

        ;; Otherwise substitute in args and rebuild
        :else
        (let [new-args (mapv #(if (scope/gat-ident? %)
                                (substitute-ident ident-map %)
                                (substitute-in-type ident-map %))
                             (:args type))]
          (core/alg-type head-result new-args (:sort type)))))
    type))

(defn substitute-in-term
  "Recursively substitute idents in an AlgTerm using an ident map."
  [ident-map term]
  (if (core/alg-term? term)
    (let [new-head (substitute-ident ident-map (:head term))
          new-args (mapv #(cond
                            (scope/gat-ident? %)
                            (substitute-ident ident-map %)

                            (core/alg-term? %)
                            (substitute-in-term ident-map %)

                            :else %)
                         (:args term))
          new-type (substitute-in-type ident-map (:type term))]
      (if (core/alg-term? new-head)
        ;; If head was substituted with a term, compose them
        ;; For now, just return the substituted head (simplification)
        new-head
        (core/alg-term new-head new-args new-type)))
    term))

;;; ============================================================================
;;; Pushforward
;;; ============================================================================

(defn build-ident-map
  "Build an ident substitution map from a type/term mapping and a context.

  Given:
  - type-map/term-map: Maps from domain idents to codomain TypeInCtx/TermInCtx
  - ctx: A TypeCtx in the domain

  Returns:
  - An ident-map for substitution"
  [mapping ctx]
  (reduce
   (fn [acc ident]
     (if-let [mapped (get mapping ident)]
       ;; Extract the actual type/term from the TypeInCtx/TermInCtx
       (let [mapped-value (cond
                            (core/type-in-ctx? mapped) (:type mapped)
                            (core/term-in-ctx? mapped) (:term mapped)
                            :else mapped)]
         (assoc acc ident mapped-value))
       acc))
   {}
   (:idents ctx)))

(defn pushforward-type
  "Apply a theory morphism to a type in context.

  Given:
  - morphism: TheoryMorphism
  - type-in-ctx: TypeInCtx in domain theory

  Returns:
  - TypeInCtx in codomain theory"
  [morphism type-in-ctx]
  {:pre [(theory-morphism? morphism)
         (core/type-in-ctx? type-in-ctx)]}
  (let [{:keys [type-map]} morphism
        ctx (:ctx type-in-ctx)
        type (:type type-in-ctx)

        ;; Build substitution map:
        ;; 1. Map type constructors from type-map
        ;; 2. Map context variables
        type-ident-map (into {}
                             (map (fn [[k v]]
                                    [k (:type v)])
                                  type-map))
        ctx-ident-map (build-ident-map type-map ctx)
        ident-map (merge type-ident-map ctx-ident-map)

        ;; Substitute in the type
        new-type (substitute-in-type ident-map type)]

    ;; Return type in an empty context (simplification)
    ;; Full implementation would push forward the context too
    (core/type-in-ctx (core/type-ctx) new-type)))

(defn pushforward-term
  "Apply a theory morphism to a term in context.

  Given:
  - morphism: TheoryMorphism
  - term-in-ctx: TermInCtx in domain theory

  Returns:
  - TermInCtx in codomain theory"
  [morphism term-in-ctx]
  {:pre [(theory-morphism? morphism)
         (core/term-in-ctx? term-in-ctx)]}
  (let [{:keys [type-map term-map]} morphism
        ctx (:ctx term-in-ctx)
        term (:term term-in-ctx)

        ;; Build substitution map:
        ;; 1. Map type/term constructors
        ;; 2. Map context variables
        type-ident-map (into {}
                             (map (fn [[k v]]
                                    [k (:type v)])
                                  type-map))
        term-ident-map (into {}
                             (map (fn [[k v]]
                                    [k (:term v)])
                                  term-map))
        ctx-type-map (build-ident-map type-map ctx)
        ctx-term-map (build-ident-map term-map ctx)
        ident-map (merge type-ident-map term-ident-map ctx-type-map ctx-term-map)

        ;; Substitute in the term
        new-term (substitute-in-term ident-map term)]

    ;; Return term in an empty context (simplification)
    (core/term-in-ctx (core/type-ctx) new-term)))

;;; ============================================================================
;;; Morphism Macro
;;; ============================================================================

(defn parse-morphism-decl
  "Parse a morphism declaration.

  Forms:
    (Ob => Ob)                                    ; Type mapping
    (Hom [a Ob, b Ob] => (Hom b a))              ; Dependent type mapping
    (id [a Ob] => (id a))                         ; Term mapping"
  [dom-gat codom-gat [lhs => rhs]]
  (when-not (= '=> =>)
    (throw (ex-info "Morphism declaration must use =>"
                    {:lhs lhs :rhs rhs})))

  (cond
    ;; Simple type mapping: Ob => Ob
    (and (symbol? lhs) (symbol? rhs))
    (let [;; Find the type in domain
          dom-type-tic (core/get-type-constructor dom-gat lhs)
          ;; Find the type in codomain
          codom-type-tic (core/get-type-constructor codom-gat rhs)]
      (when-not dom-type-tic
        (throw (ex-info (str "Type not found in domain: " lhs)
                        {:type lhs})))
      (when-not codom-type-tic
        (throw (ex-info (str "Type not found in codomain: " rhs)
                        {:type rhs})))
      [:type (-> dom-type-tic :type :head) codom-type-tic])

    ;; Dependent type mapping: (Hom [a Ob, b Ob] => (Hom b a))
    (and (seq? lhs) (seq? rhs))
    (let [[lhs-head & lhs-rest] lhs
          [rhs-head & rhs-args] rhs]
      ;; This is a simplification - full implementation would need to:
      ;; 1. Parse the context in lhs-rest
      ;; 2. Build the substitution
      ;; 3. Create the mapped type/term with proper context
      (throw (ex-info "Complex morphism mappings not yet implemented"
                      {:lhs lhs :rhs rhs})))

    :else
    (throw (ex-info "Invalid morphism declaration"
                    {:lhs lhs :rhs rhs}))))

(defmacro defmorphism
  "Define a theory morphism.

  Syntax:
    (defmorphism MorphismName [DomainTheory => CodomainTheory]
      (TypeInDomain => TypeInCodomain)
      (TermInDomain => TermInCodomain)
      ...)

  Example:
    (defmorphism OpCategory [Category => Category]
      (Ob => Ob)
      (Hom => Hom)  ; Simplified - full version would swap arguments
      (compose => compose)
      (id => id))"
  [morphism-name [dom-theory-name => codom-theory-name] & mappings]
  (when-not (= '=> =>)
    (throw (ex-info "Morphism signature must use =>"
                    {:sig [dom-theory-name => codom-theory-name]})))

  `(def ~morphism-name
     (let [dom-gat# ~dom-theory-name
           codom-gat# ~codom-theory-name
           type-map# {}
           term-map# {}]
       ;; Process mappings
       ;; (This is simplified - full implementation would parse each mapping)
       (theory-morphism '~morphism-name dom-gat# codom-gat# type-map# term-map#))))

;;; ============================================================================
;;; Wellformedness Validation
;;; ============================================================================
;;; Modern GATlab.jl removed its bespoke static type checker in Feb 2025
;;; (commit a1c16f8). What remains — and what we still need — is *morphism
;;; wellformedness*: every domain sort/term is mapped, every target exists in
;;; the codomain, and contexts match arity. These checks were previously in
;;; katzen.validation, now folded here next to the morphism record.

(defn validate-type-mapping
  "Validate that a domain type maps to a valid codomain TypeInCtx.

  Checks:
  1. Target type exists in the codomain theory
  2. Domain and codomain contexts have matching length

  Returns nil on success; throws ex-info on error."
  [dom-gat codom-gat dom-type-ident codom-type-in-ctx]
  (let [dom-type-tic (core/get-type-constructor dom-gat (:name dom-type-ident))]
    (when-not dom-type-tic
      (throw (ex-info (str "Type not found in domain: " (:name dom-type-ident))
                      {:type dom-type-ident})))
    (let [codom-type    (:type codom-type-in-ctx)
          codom-type-tic (core/get-type-constructor codom-gat (-> codom-type :head :name))]
      (when-not codom-type-tic
        (throw (ex-info (str "Type not found in codomain: " (-> codom-type :head :name))
                        {:type      (-> codom-type :head :name)
                         :codomain  (:name codom-gat)})))
      (let [dom-len   (core/context-length (:ctx dom-type-tic))
            codom-len (core/context-length (:ctx codom-type-in-ctx))]
        (when-not (= dom-len codom-len)
          (throw (ex-info "Type mapping has mismatched context lengths"
                          {:domain-type            (:name dom-type-ident)
                           :domain-context-length  dom-len
                           :codomain-context-length codom-len})))))))

(defn validate-term-mapping
  "Validate that a domain term maps to a valid codomain TermInCtx.

  Same checks as validate-type-mapping but for terms.

  Returns nil on success; throws ex-info on error."
  [dom-gat codom-gat dom-term-ident codom-term-in-ctx]
  (let [dom-term-tic (core/get-term-constructor dom-gat (:name dom-term-ident))]
    (when-not dom-term-tic
      (throw (ex-info (str "Term not found in domain: " (:name dom-term-ident))
                      {:term dom-term-ident})))
    (let [codom-term    (:term codom-term-in-ctx)
          codom-term-tic (core/get-term-constructor codom-gat (-> codom-term :head :name))]
      (when-not codom-term-tic
        (throw (ex-info (str "Term not found in codomain: " (-> codom-term :head :name))
                        {:term     (-> codom-term :head :name)
                         :codomain (:name codom-gat)})))
      (let [dom-len   (core/context-length (:ctx dom-term-tic))
            codom-len (core/context-length (:ctx codom-term-in-ctx))]
        (when-not (= dom-len codom-len)
          (throw (ex-info "Term mapping has mismatched context lengths"
                          {:domain-term            (:name dom-term-ident)
                           :domain-context-length  dom-len
                           :codomain-context-length codom-len})))))))

(defn validate-theory-morphism
  "Validate every mapping in a TheoryMorphism.

  Returns the morphism on success; throws ex-info on the first invalid mapping."
  [morphism]
  (let [{:keys [dom codom type-map term-map]} morphism]
    (doseq [[dom-ident codom-tic] type-map]
      (validate-type-mapping dom codom dom-ident codom-tic))
    (doseq [[dom-ident codom-tic] term-map]
      (validate-term-mapping dom codom dom-ident codom-tic))
    morphism))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(defn format-morphism
  "Pretty-print a theory morphism."
  [morphism]
  (let [sb (StringBuilder.)]
    (.append sb (str "Morphism: " (:name morphism) "\n"))
    (.append sb (str "  " (:name (:dom morphism)) " => "
                     (:name (:codom morphism)) "\n\n"))

    (when (seq (:type-map morphism))
      (.append sb "Type Mappings:\n")
      (doseq [[dom-ident codom-tic] (:type-map morphism)]
        (.append sb (str "  " (:name dom-ident) " => "
                         (-> codom-tic :type :head :name) "\n"))))

    (when (seq (:term-map morphism))
      (.append sb "\nTerm Mappings:\n")
      (doseq [[dom-ident codom-tic] (:term-map morphism)]
        (.append sb (str "  " (:name dom-ident) " => "
                         (-> codom-tic :term :head :name) "\n"))))

    (str sb)))

;;; ============================================================================
;;; Identity Morphisms
;;; ============================================================================

(defrecord IdTheoryMap [gat]
  Object
  (toString [_]
    (str "IdTheoryMap(" (:name gat) ")")))

(defn id-theory-map
  "Create an identity theory morphism.

  The identity morphism maps each type and term to itself."
  [gat]
  {:pre [(core/gat? gat)]}
  (->IdTheoryMap gat))

(defn id-theory-map?
  "Check if value is an IdTheoryMap."
  [x]
  (instance? IdTheoryMap x))

;;; ============================================================================
;;; Theory Inclusions
;;; ============================================================================

(defrecord TheoryIncl [dom codom]
  Object
  (toString [_]
    (str "TheoryIncl(" (:name dom) " ⊆ " (:name codom) ")")))

(defn theory-subsumes?
  "Check if theory B subsumes theory A (A ⊆ B).

  Theory A is subsumed by B if:
  - All type constructors in A exist in B (by name)
  - All term constructors in A exist in B (by name)

  This is a simplified check - full implementation would check
  that the types/terms have compatible signatures."
  [theory-a theory-b]
  (let [a-types (set (map (comp :name :head :type) (:type-constructors theory-a)))
        b-types (set (map (comp :name :head :type) (:type-constructors theory-b)))
        a-terms (set (map (comp :name :head :term) (:term-constructors theory-a)))
        b-terms (set (map (comp :name :head :term) (:term-constructors theory-b)))]
    (and (set/subset? a-types b-types)
         (set/subset? a-terms b-terms))))

(defn theory-incl
  "Create a theory inclusion morphism.

  Theory inclusions represent the inclusion of a smaller theory into a larger one.
  The domain must be subsumed by the codomain (dom ⊆ codom).

  Example: ThGraph ⊆ ThCategory (every graph is a category)"
  [dom-gat codom-gat]
  {:pre [(core/gat? dom-gat)
         (core/gat? codom-gat)]}
  (if (theory-subsumes? dom-gat codom-gat)
    (->TheoryIncl dom-gat codom-gat)
    (throw (ex-info "Cannot construct TheoryInclusion - domain not subsumed by codomain"
                    {:dom (:name dom-gat)
                     :codom (:name codom-gat)}))))

(defn theory-incl?
  "Check if value is a TheoryIncl."
  [x]
  (instance? TheoryIncl x))

;;; ============================================================================
;;; Domain and Codomain Accessors
;;; ============================================================================

(defn dom
  "Get the domain of a theory morphism."
  [morphism]
  (cond
    (theory-morphism? morphism) (:dom morphism)
    (id-theory-map? morphism) (:gat morphism)
    (theory-incl? morphism) (:dom morphism)
    :else (throw (ex-info "Unknown morphism type" {:morphism morphism}))))

(defn codom
  "Get the codomain of a theory morphism."
  [morphism]
  (cond
    (theory-morphism? morphism) (:codom morphism)
    (id-theory-map? morphism) (:gat morphism)
    (theory-incl? morphism) (:codom morphism)
    :else (throw (ex-info "Unknown morphism type" {:morphism morphism}))))

;;; ============================================================================
;;; Morphism Composition
;;; ============================================================================

(defn compose-morphisms
  "Compose two theory morphisms. DIAGRAMMATIC order, the shared katzen
  convention (see `katzen.cat/compose`): the FIRST argument is applied
  first. `(compose-morphisms f g)` maps dom(f) → codom(g), acting as
  `g ∘ f`; requires codom(f) = dom(g)."
  [f g]
  (when-not (= (codom f) (dom g))
    (throw (ex-info "Cannot compose morphisms - codomain of f must equal domain of g"
                    {:codom-f (:name (codom f))
                     :dom-g (:name (dom g))})))

  (cond
    ;; Identity laws: id ∘ id = id
    (and (id-theory-map? f) (id-theory-map? g))
    f

    ;; Identity laws: id ∘ g = g
    (id-theory-map? f)
    g

    ;; Identity laws: f ∘ id = f
    (id-theory-map? g)
    f

    ;; Inclusion composition: TheoryIncl ∘ TheoryIncl
    (and (theory-incl? f) (theory-incl? g))
    (theory-incl (dom f) (codom g))

    ;; General case: compose by composing the mappings
    ;; This is simplified - full implementation would properly compose the maps
    (and (theory-morphism? f) (theory-morphism? g))
    (throw (ex-info "General morphism composition not yet implemented"
                    {:f f :g g}))

    :else
    (throw (ex-info "Unsupported morphism composition"
                    {:f f :g g}))))
