(ns katzen.presentation
  "Presentations of GATs.

  A presentation is a finite collection of generators and equations
  that define a model of a GAT. Generators are named instances of
  types in the theory, and equations constrain the relationships
  between generators.

  Example:
    (defpresentation GraphPresentation Graph
      (generator V :V)
      (generator E :E)
      (generator src :src)
      (generator tgt :tgt)
      (equation (src v) w))"
  (:require [katzen.core :as core]
            [katzen.scope :as scope]
            [clojure.set :as set]))

;;; ============================================================================
;;; Presentation Data Structure
;;; ============================================================================

(defrecord Presentation [theory generators generator-index equations]
  Object
  (toString [_]
    (str "Presentation(" (:name theory) ", "
         (count generator-index) " generators, "
         (count equations) " equations)")))

(defn presentation
  "Create a new presentation for a GAT.

  Parameters:
    theory - The GAT this presentation is for
    generators - Map from generator names to generator info
                 {name {:type type-name :index idx}}
    generator-index - Map from generator names to their info
                      (same as generators, kept for API consistency)
    equations - Vector of equation pairs [{:lhs term :rhs term}]"
  [theory generators generator-index equations]
  {:pre [(core/gat? theory)
         (map? generators)
         (map? generator-index)
         (vector? equations)]}
  (->Presentation theory generators generator-index equations))

(defn presentation?
  "Check if value is a Presentation."
  [x]
  (instance? Presentation x))

(defn empty-presentation
  "Create an empty presentation for a theory."
  [theory]
  {:pre [(core/gat? theory)]}
  (->Presentation theory {} {} []))

;;; ============================================================================
;;; Generator Management
;;; ============================================================================

(defn has-generator?
  "Check if a generator with the given name exists in the presentation."
  [pres name]
  {:pre [(presentation? pres)
         (symbol? name)]}
  (contains? (:generator-index pres) name))

(defn get-generator
  "Get generator info by name.

  Returns a map with :name, :type, and :index.
  Throws if generator not found."
  [pres name]
  {:pre [(presentation? pres)
         (symbol? name)]}
  (if-let [gen-info (get (:generator-index pres) name)]
    gen-info
    (throw (ex-info (str "Generator not found: " name)
                    {:name name
                     :available (keys (:generator-index pres))}))))

(defn generators
  "Get all generators in the presentation.

  With no type argument, returns all generators as a sequence of maps.
  With a type argument, returns only generators of that type."
  ([pres]
   {:pre [(presentation? pres)]}
   (vals (:generators pres)))
  ([pres type-name]
   {:pre [(presentation? pres)
          (symbol? type-name)]}
   (filter #(= type-name (:type %)) (vals (:generators pres)))))

(defn generator-index
  "Get the index of a generator within its type.

  Returns the position (0-based) of this generator among all generators
  of the same type."
  [pres name]
  {:pre [(presentation? pres)
         (symbol? name)]}
  (:index (get-generator pres name)))

(defn add-generator!
  "Add a generator to a presentation.

  Parameters:
    pres - Presentation to modify
    name - Symbol naming the generator
    type-name - Symbol naming the GAT type

  Returns:
    New presentation with generator added.

  Note: This is a functional update - returns new presentation.
  The ! is kept for API consistency with GATlab.jl."
  [pres name type-name]
  {:pre [(presentation? pres)
         (symbol? name)
         (symbol? type-name)]}

  ;; Check if generator already exists
  (when (has-generator? pres name)
    (throw (ex-info (str "Generator already exists: " name)
                    {:name name :type type-name})))

  ;; Check if type exists in theory
  (let [theory (:theory pres)
        type-tic (core/get-type-constructor theory type-name)]
    (when-not type-tic
      (throw (ex-info (str "Type not found in theory: " type-name)
                      {:type type-name :theory (:name theory)}))))

  ;; Calculate index: count existing generators of this type
  (let [existing-of-type (generators pres type-name)
        idx (count existing-of-type)
        gen-info {:name name :type type-name :index idx}
        new-generators (assoc (:generators pres) name gen-info)
        new-index (assoc (:generator-index pres) name gen-info)]
    (->Presentation (:theory pres) new-generators new-index (:equations pres))))

(defn add-generators!
  "Add multiple generators to a presentation.

  Parameters:
    pres - Presentation to modify
    gen-specs - Sequence of [name type-name] pairs

  Returns:
    New presentation with all generators added."
  [pres gen-specs]
  {:pre [(presentation? pres)
         (sequential? gen-specs)]}
  (reduce
   (fn [p [name type-name]]
     (add-generator! p name type-name))
   pres
   gen-specs))

;;; ============================================================================
;;; Equation Management
;;; ============================================================================

(defn equations
  "Get all equations in the presentation.

  Returns a vector of equation maps with :lhs and :rhs terms."
  [pres]
  {:pre [(presentation? pres)]}
  (:equations pres))

(defn add-equation!
  "Add an equation to a presentation.

  Parameters:
    pres - Presentation to modify
    lhs - Left-hand side term
    rhs - Right-hand side term

  Returns:
    New presentation with equation added.

  Note: This is a functional update - returns new presentation.
  The ! is kept for API consistency with GATlab.jl."
  [pres lhs rhs]
  {:pre [(presentation? pres)]}

  ;; Validate that lhs and rhs are valid terms
  ;; (Simplified - full implementation would check types match)
  (let [new-equation {:lhs lhs :rhs rhs}
        new-equations (conj (:equations pres) new-equation)]
    (->Presentation (:theory pres)
                    (:generators pres)
                    (:generator-index pres)
                    new-equations)))

(defn add-equations!
  "Add multiple equations to a presentation.

  Parameters:
    pres - Presentation to modify
    eq-pairs - Sequence of [lhs rhs] pairs

  Returns:
    New presentation with all equations added."
  [pres eq-pairs]
  {:pre [(presentation? pres)
         (sequential? eq-pairs)]}
  (reduce
   (fn [p [lhs rhs]]
     (add-equation! p lhs rhs))
   pres
   eq-pairs))

;;; ============================================================================
;;; Definition Management
;;; ============================================================================

(defn add-definition!
  "Add a generator with a defining equation.

  This is a convenience function that:
  1. Adds a generator
  2. Adds an equation defining that generator

  Parameters:
    pres - Presentation to modify
    name - Generator name
    type-name - Generator type
    rhs - Right-hand side of defining equation

  Returns:
    New presentation with generator and equation added."
  [pres name type-name rhs]
  {:pre [(presentation? pres)
         (symbol? name)
         (symbol? type-name)]}
  (let [tag (scope/scope-tag)
        ;; Get the type from the theory
        theory (:theory pres)
        type-tic (core/get-type-constructor theory type-name)
        the-type (:type type-tic)
        lhs-ident (scope/ident tag 0 name)
        lhs (core/alg-term lhs-ident [] the-type)]
    (-> pres
        (add-generator! name type-name)
        (add-equation! lhs rhs))))

;;; ============================================================================
;;; @present Macro
;;; ============================================================================

(defn parse-generator-decl
  "Parse a generator declaration.

  Forms:
    (gen-name :type-name)
    => {:type :generator :name gen-name :type-name type-name}"
  [[name _arrow type-name]]
  (when-not (and (symbol? name) (= _arrow :-) (symbol? type-name))
    (throw (ex-info "Invalid generator declaration"
                    {:decl [name _arrow type-name]})))
  {:type :generator
   :name name
   :type-name type-name})

(defn parse-equation-decl
  "Parse an equation declaration.

  Forms:
    (= lhs rhs)
    => {:type :equation :lhs lhs :rhs rhs}"
  [[eq lhs rhs]]
  (when-not (= eq '=)
    (throw (ex-info "Expected = for equation" {:form [eq lhs rhs]})))
  {:type :equation
   :lhs lhs
   :rhs rhs})

(defn parse-present-stmt
  "Parse a statement in a @present block.

  Recognizes:
  - Generator: (name :- Type)
  - Equation: (= lhs rhs)"
  [stmt]
  (cond
    ;; Generator: (x :- V)
    (and (list? stmt)
         (= 3 (count stmt))
         (= :- (second stmt)))
    (parse-generator-decl stmt)

    ;; Equation: (= lhs rhs)
    (and (list? stmt)
         (= 3 (count stmt))
         (= '= (first stmt)))
    (parse-equation-decl stmt)

    :else
    (throw (ex-info "Unknown statement in @present"
                    {:stmt stmt}))))

(defmacro defpresentation
  "Define a presentation for a theory.

  Syntax:
    (defpresentation PresentationName TheoryName
      (gen1 :- Type1)
      (gen2 :- Type2)
      (= lhs rhs))

  Example:
    (defpresentation GraphPres Graph
      (v1 :- V)
      (v2 :- V)
      (e1 :- E))"
  [pres-name theory-name & stmts]
  (let [parsed-stmts (map parse-present-stmt stmts)
        generators (filter #(= :generator (:type %)) parsed-stmts)
        equations (filter #(= :equation (:type %)) parsed-stmts)]
    `(def ~pres-name
       (let [base# (empty-presentation ~theory-name)]
         (-> base#
             ~@(for [gen generators]
                 `(add-generator! '~(:name gen) '~(:type-name gen)))
             ;; Note: Equations would need full term parsing which is complex
             ;; For now, we just support generator declarations
             )))))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(defn format-presentation
  "Pretty-print a presentation."
  [pres]
  (let [sb (StringBuilder.)
        theory (:theory pres)]
    (.append sb (str "Presentation: " (:name theory) "\n\n"))

    ;; Group generators by type
    (let [gens-by-type (group-by :type (vals (:generators pres)))]
      (.append sb "Generators:\n")
      (doseq [[type-name gens] (sort-by key gens-by-type)]
        (.append sb (str "  " type-name ":\n"))
        (doseq [gen (sort-by :index gens)]
          (.append sb (str "    " (:name gen) "\n")))))

    ;; Show equations
    (when (seq (:equations pres))
      (.append sb "\nEquations:\n")
      (doseq [eq (:equations pres)]
        (.append sb (str "  " (:lhs eq) " = " (:rhs eq) "\n"))))

    (str sb)))

;;; ============================================================================
;;; Presentation Inheritance
;;; ============================================================================

(defn merge-presentations
  "Merge two presentations.

  Combines generators and equations from both presentations.
  The presentations must be for the same theory.

  Throws if:
  - Presentations are for different theories
  - Generator names conflict

  Returns:
    New presentation with combined generators and equations."
  [pres1 pres2]
  {:pre [(presentation? pres1)
         (presentation? pres2)]}

  (when-not (= (:theory pres1) (:theory pres2))
    (throw (ex-info "Cannot merge presentations for different theories"
                    {:theory1 (:name (:theory pres1))
                     :theory2 (:name (:theory pres2))})))

  ;; Check for name conflicts
  (let [names1 (set (keys (:generator-index pres1)))
        names2 (set (keys (:generator-index pres2)))
        conflicts (set/intersection names1 names2)]
    (when (seq conflicts)
      (throw (ex-info "Generator name conflicts when merging presentations"
                      {:conflicts conflicts}))))

  ;; Merge generators and equations
  (let [merged-generators (merge (:generators pres1) (:generators pres2))
        merged-index (merge (:generator-index pres1) (:generator-index pres2))
        merged-equations (vec (concat (:equations pres1) (:equations pres2)))]
    (->Presentation (:theory pres1)
                    merged-generators
                    merged-index
                    merged-equations)))

(defn extend-presentation
  "Extend a presentation with new generators and equations.

  This creates a new presentation that inherits all generators and
  equations from the base, then adds new ones.

  Parameters:
    base - Base presentation to extend
    gen-specs - Sequence of [name type-name] pairs for new generators
    eq-pairs - Sequence of [lhs rhs] pairs for new equations

  Returns:
    New extended presentation."
  [base gen-specs eq-pairs]
  {:pre [(presentation? base)
         (sequential? gen-specs)
         (sequential? eq-pairs)]}
  (-> base
      (add-generators! gen-specs)
      (add-equations! eq-pairs)))
