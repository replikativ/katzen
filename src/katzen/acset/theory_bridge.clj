(ns katzen.acset.theory-bridge
  "Bridge ACSet schemas + SchemaMorphisms to katzen.theory / katzen.morphism,
   enabling `katzen.ansatz.export/check-morphism!` to verify ACSet schema
   migrations via the Lean kernel.

   Two converters:
     - `schema->theory`              : schema map → katzen GAT
     - `schema-morphism->theory-morphism` : SchemaMorphism → TheoryMorphism

   And one user-facing entry point:
     - `verify-schema-morphism!`     : validates F at the ACSet level, then
                                       bridges to the theory level and runs
                                       ansatz/check-morphism! against any
                                       axioms declared on the schemas.

   Bridge limitations (v1):
     - Objects become Sort-valued type constructors with no arguments
       (the standard `(type X)` form).
     - Homs and attrs become *unary* term constructors `(term f :ctx [x dom] :ret codom)`.
     - SchemaMorphism paths must have length 1; empty paths (identity-on-
       object) would require synthesizing an identity term that schemas
       don't expose, and is deferred.
     - Compound RHS-of-axiom forms are passed through unchanged — the
       schema author is responsible for using term-symbols that exist
       in their theory.

   For axiom-free schemas the check is a no-op beyond the existing
   `validate-schema-morphism!`; the real payoff is for axiomized schemas
   (e.g. symmetric graphs with `inv ∘ inv = id`) where axiom preservation
   is non-trivial."
  (:require [katzen.acset.check :as check]
            [katzen.acset.migration :as mig]
            [katzen.core :as core]
            [katzen.morphism :as morph]
            [katzen.theory :as theory]))

;; ============================================================================
;; schema → theory
;; ============================================================================

(defn- as-sym
  "Schema labels are keywords; deftheory wants symbols. `(as-sym :E)` → `E`."
  [k]
  (cond
    (symbol? k) k
    (keyword? k) (symbol (name k))
    :else (throw (ex-info "Schema label must be a keyword or symbol" {:got k}))))

(defn- hom-decl
  "(term <name> :ctx [x <dom>] :ret <codom>) for a hom or attr spec."
  [{n :name dom :dom codom :codom}]
  (list 'term (as-sym n)
        :ctx [(symbol "x") (as-sym dom)]
        :ret (as-sym codom)))

(defn- axiom-decl
  "Convert an axiom spec {:name :ctx :lhs :rhs} to the deftheory form."
  [{n :name ctx :ctx lhs :lhs rhs :rhs}]
  (list 'axiom (as-sym n)
        :ctx (vec (mapcat (fn [{vname :name vtype :type}]
                            [(as-sym vname) (as-sym vtype)])
                          ctx))
        (list '= lhs rhs)))

(defn schema->theory
  "Convert a schema map to a katzen GAT (a runtime theory value).

   Recognised schema fields:
     :name        symbol or keyword
     :objects     vector of object labels (keywords or symbols)
     :homs        vector of {:name :dom :codom}
     :attr-types  vector of attr-type labels
     :attrs       vector of {:name :dom :codom}
     :axioms      vector of {:name :ctx :lhs :rhs} (optional)

   Axiom ctx is a vec of {:name :type} maps; lhs and rhs are s-expressions
   in the surface syntax of `deftheory` (using the symbol names of objects
   and homs in the theory)."
  [schema]
  (let [name      (as-sym (or (:name schema) 'AnonSchema))
        type-decls      (for [O (:objects schema)]      (list 'type (as-sym O)))
        attr-type-decls (for [T (:attr-types schema)]   (list 'type (as-sym T)))
        hom-decls       (mapv hom-decl (:homs schema))
        attr-decls      (mapv hom-decl (:attrs schema))
        axiom-decls     (mapv axiom-decl (:axioms schema))
        decls (vec (concat type-decls attr-type-decls hom-decls attr-decls axiom-decls))]
    (:gat (theory/parse-theory name decls))))

;; ============================================================================
;; SchemaMorphism → TheoryMorphism
;; ============================================================================

(defn- one-step
  "Validate the path is length 1 and return its single element."
  [path label]
  (when-not (and (vector? path) (= 1 (count path)))
    (throw (ex-info "Theory-bridge requires single-step hom/attr paths"
                    {:label label :path path
                     :hint "v1 doesn't yet handle empty paths (identity-on-object) or multi-step compositions"})))
  (first path))

(defn- type-head [tic]
  (-> tic :type :head))

(defn- term-head [tic]
  (-> tic :term :head))

(defn schema-morphism->theory-morphism
  "Convert a SchemaMorphism F to a TheoryMorphism, given the pre-built
   dom and codom katzen GATs."
  [F dom-theory codom-theory]
  (let [{:keys [name ob-map hom-map attr-map]} F
        dom-schema (:dom F)
        type-map
        (into {}
              (for [O (concat (:objects dom-schema) (:attr-types dom-schema))]
                (let [dom-O-sym (as-sym O)
                      codom-O   (or (get ob-map O)
                                    ;; attr-types share the type-map slot; allow id-on-attr-type
                                    O)
                      codom-O-sym (as-sym codom-O)
                      dom-tc   (core/get-type-constructor dom-theory dom-O-sym)
                      codom-tc (core/get-type-constructor codom-theory codom-O-sym)]
                  (when-not dom-tc
                    (throw (ex-info "dom type constructor missing"
                                    {:object O :dom-theory (:name dom-theory)})))
                  (when-not codom-tc
                    (throw (ex-info "codom type constructor missing"
                                    {:object codom-O :codom-theory (:name codom-theory)})))
                  [(type-head dom-tc) codom-tc])))
        term-map
        (into {}
              (concat
               (for [{n :name} (:homs dom-schema)]
                 (let [codom-hom (one-step (get hom-map n) n)
                       dom-tc   (core/get-term-constructor dom-theory (as-sym n))
                       codom-tc (core/get-term-constructor codom-theory
                                                           (as-sym codom-hom))]
                   (when-not dom-tc
                     (throw (ex-info "dom term constructor missing"
                                     {:hom n :dom-theory (:name dom-theory)})))
                   (when-not codom-tc
                     (throw (ex-info "codom term constructor missing"
                                     {:hom codom-hom :codom-theory (:name codom-theory)})))
                   [(term-head dom-tc) codom-tc]))
               (for [{n :name} (:attrs dom-schema)]
                 (let [codom-attr (one-step (get attr-map n) n)
                       dom-tc   (core/get-term-constructor dom-theory (as-sym n))
                       codom-tc (core/get-term-constructor codom-theory
                                                           (as-sym codom-attr))]
                   [(term-head dom-tc) codom-tc]))))]
    (morph/theory-morphism (as-sym (or name 'AnonMorphism))
                           dom-theory codom-theory
                           type-map term-map)))

;; ============================================================================
;; verify-schema-morphism!
;; ============================================================================

(defn verify-schema-morphism!
  "Validate F at the ACSet level, then bridge it to katzen theories and
   run `ansatz/check-morphism!` on the result.

   Options:
     :verify?  :strict (default) — unresolved axioms throw
                :informational    — unresolved axioms log and return :unresolved

   Returns F on success; throws (or returns :unresolved in informational
   mode) on the first unresolved axiom obligation."
  ([F] (verify-schema-morphism! F {}))
  ([F opts]
   (mig/validate-schema-morphism! F)
   (let [dom-thy (schema->theory (:dom F))
         cod-thy (schema->theory (:codom F))
         thy-m   (schema-morphism->theory-morphism F dom-thy cod-thy)
         check!  (requiring-resolve 'katzen.ansatz.export/check-morphism!)]
     (when-not check!
       (throw (ex-info "ansatz not on classpath — verify-schema-morphism! requires the :ansatz alias"
                       {})))
     (check! thy-m opts)
     F)))

;; ============================================================================
;; Verified migration wrappers
;; ============================================================================

(defn verified-migrate
  "Like `katzen.acset.migration/migrate` but with two extra guarantees:

     - F is bridged to a theory morphism and `check-morphism!`-verified
       against its codomain's installed CIC encoding (the categorical
       half — axiom preservation).
     - Optionally, the input X is checked against its schema's axioms
       before migration and the output Y against the dom-schema's
       axioms after. The categorical theorem says the post-check is
       redundant whenever the pre-check passes and F is verified —
       it's a defensive backstop against implementation bugs in the
       migration loop.

   Options:
     :verify?         passed through to verify-schema-morphism!
     :check-instance? truthy (default) — run check-axioms! on X and Y
     other opts       passed through to migrate"
  ([F X] (verified-migrate F X {}))
  ([F X opts]
   (let [check-instance? (get opts :check-instance? true)]
     (verify-schema-morphism! F (select-keys opts [:verify?]))
     (when check-instance? (check/check-axioms! X))
     (let [Y (mig/migrate F X (dissoc opts :verify? :check-instance?))]
       (when check-instance? (check/check-axioms! Y))
       Y))))

(defn verified-migrate-dynamics
  "Like `katzen.petri.migration/migrate-dynamics` but verifies F first.
   See `verified-migrate` for the `:check-instance?` option — it's
   applied to the PetriDynamics' underlying net here too."
  ([F dyn] (verified-migrate-dynamics F dyn {}))
  ([F dyn opts]
   (let [check-instance? (get opts :check-instance? true)]
     (verify-schema-morphism! F (select-keys opts [:verify?]))
     (when check-instance? (check/check-axioms! (:net dyn)))
     (let [result ((requiring-resolve 'katzen.petri.migration/migrate-dynamics) F dyn)]
       (when check-instance? (check/check-axioms! (:net result)))
       result))))
