(ns katzen.ansatz.export
  "Export katzen GAT values into ansatz / Lean 4 CIC declarations for
   kernel-checked verification.

   This namespace is loaded only when the optional :ansatz alias is on
   the classpath. The main katzen.* namespaces never depend on ansatz.

   API summary (incremental — built across phase 2):
     check-theory!   — type-check a theory's encoding in CIC
     check-instance! — verify an instance's witness terms for axioms (2f)
     check-morphism! — verify functoriality of a theory morphism (2g)
     export-to-lean! — write Lean 4 source (deferred)

   Encoding strategy:
     A theory becomes an inductive type with one constructor `mk` whose
     fields are the theory's term constructors and axiom witnesses.

       (deftheory ThMonoid                 ;; katzen side
         (type El)
         (term mul :ctx [x El, y El] :ret El)
         (term unit :ret El))

     becomes (CIC):

       inductive ThMonoidStr (El : Type) where
         | mk : (mul : El → El → El) → (unit : El) → ThMonoidStr El

     Sorts are inductive parameters; terms are constructor fields.
     Dependent sorts (Hom : Ob → Ob → Type) become parameters of arrow
     type. Axioms become forall-quantified Eq fields (added in 2e).

   The kernel verification is non-destructive: the ansatz global env is
   captured before and restored after each call so theories can be
   checked independently without polluting the session."
  (:require [ansatz.core :as a]
            [ansatz.inductive :as a-ind]
            [ansatz.kernel.env :as a-env]
            [ansatz.kernel.expr :as a-expr]
            [ansatz.kernel.name :as a-name]
            [ansatz.kernel.tc :as a-tc]
            [ansatz.surface.term :as a-term]
            [katzen.core :as core]
            [katzen.morphism :as morphism]
            [katzen.scope :as scope]))

;; -------------------------------------------------------------------
;; Theory → s-expr translation
;; -------------------------------------------------------------------

(defn- type-ctor-name
  "Sort name from a type constructor (TypeInCtx)."
  [type-in-ctx]
  (-> type-in-ctx :type :head :name))

(defn- type-ctor-arg-sort-names
  "For a sort declared with parameters (e.g. Hom [dom Ob, codom Ob]),
   return the parameter types' head names. Empty for non-dependent sorts."
  [type-in-ctx]
  (mapv #(-> % :head :name) (-> type-in-ctx :ctx :types)))

(defn- sort->param-spec
  "Type constructor → flat [name type-expr] pair for ansatz inductive params.

   Non-dependent sort (e.g. ThMonoid's El) → [El Type]
   Dependent sort (e.g. ThCategory's Hom : Ob → Ob → Type)
     → [Hom (-> Ob (-> Ob Type))]"
  [type-in-ctx]
  (let [nm (type-ctor-name type-in-ctx)
        arg-sorts (type-ctor-arg-sort-names type-in-ctx)]
    (if (empty? arg-sorts)
      [nm 'Type]
      [nm (reduce (fn [acc s] (list '-> s acc))
                  'Type
                  (reverse arg-sorts))])))

(defn- alg-term->form
  "AlgTerm → s-expr. Term applications are lists `(head arg1 arg2 ...)`
   where each arg is recursively converted. Idents become bare symbols.

   This is used inside type expressions like `(Hom a b)` where `a` and
   `b` are bvar references from the enclosing forall context."
  [alg-term]
  (let [head (-> alg-term :head :name)
        args (:args alg-term)]
    (if (empty? args)
      head
      (cons head
            (map (fn [a]
                   (cond
                     (core/alg-term? a) (alg-term->form a)
                     (scope/gat-ident? a) (:name a)
                     :else a))
                 args)))))

(defn- alg-type->form
  "AlgType → s-expr. Sort references with no args become bare symbols
   (e.g. `El`); sort references with args become applications (e.g.
   `(Hom a b)`)."
  [alg-type]
  (let [head-name (-> alg-type :head :name)
        args (:args alg-type)]
    (if (empty? args)
      head-name
      (cons head-name
            (map (fn [a]
                   (cond
                     (core/alg-term? a) (alg-term->form a)
                     (scope/gat-ident? a) (:name a)
                     (core/alg-type? a) (alg-type->form a)
                     :else a))
                 args)))))

(defn- ctx->binders
  "TypeCtx → flat [name1 type1 name2 type2 ...] suitable for the body of
   `(forall [...] body)`. Each binding pairs an ident with its AlgType."
  [ctx]
  (vec
   (mapcat (fn [ident alg-type]
             [(:name ident) (alg-type->form alg-type)])
           (:idents ctx)
           (:types ctx))))

(defn- term->field-form
  "Term constructor → s-expr field type for the inductive's mk constructor.

   - mul : El → El → El      (ctx [x El, y El], ret El)
       → (forall [x El, y El] El)
       — kernel will simplify forall over non-dependent binders to
         arrows automatically; we always emit forall so the same code
         handles dependent and non-dependent cases.
   - unit : El               (empty ctx, ret El)
       → El
   - compose : ∀ a b c, Hom a b → Hom b c → Hom a c
       → (forall [a Ob, b Ob, c Ob, f (Hom a b), g (Hom b c)] (Hom a c))"
  [term-in-ctx]
  (let [binders (ctx->binders (:ctx term-in-ctx))
        ret-form (alg-type->form (-> term-in-ctx :term :type))]
    (if (empty? binders)
      ret-form
      (list 'forall binders ret-form))))

(defn- arg->form
  "Convert a term-application argument into an s-expr form."
  [a]
  (cond
    (core/alg-term? a) (alg-term->form a)
    (scope/gat-ident? a) (:name a)
    :else a))

(defn- substitute-symbols
  "Substitute bare symbols in an s-expr form according to a name→form map.
   Symbols not in the map are passed through. Used to specialize a term
   constructor's declared return type to its actual call site."
  [subst form]
  (cond
    (symbol? form) (get subst form form)
    (sequential? form) (map (partial substitute-symbols subst) form)
    :else form))

(defn- term-sort-form
  "Determine the sort an AlgTerm or Ident lives in, returned as an s-expr
   ready for compile-type. katzen's axiom parser leaves AlgTerm :type
   fields as a DUMMY placeholder, so we resolve sort by lookup:

     - AlgTerm `(f a1..an)`:
       Look up `f` in the GAT's term constructors. Specialize its
       declared return type by substituting actual args for formal
       binders — needed for dependent return types like `compose`'s
       `(Hom a c)`, where `a` and `c` are the ctor's own formals that
       must be replaced with the axiom's actual values.

     - Ident: look up its name in the axiom's ctx for the declared
       binding sort."
  [gat ctx term-or-ident]
  (cond
    (core/alg-term? term-or-ident)
    (let [head-name (-> term-or-ident :head :name)
          term-ctor (some (fn [tc]
                            (when (= head-name (-> tc :term :head :name))
                              tc))
                          (:term-constructors gat))]
      (when-not term-ctor
        (throw (ex-info (str "Unknown term constructor in axiom: " head-name)
                        {:head head-name :gat (:name gat)})))
      (let [formal-names (mapv :name (-> term-ctor :ctx :idents))
            actual-forms (mapv arg->form (:args term-or-ident))
            subst (zipmap formal-names actual-forms)
            raw-ret (alg-type->form (-> term-ctor :term :type))]
        (substitute-symbols subst raw-ret)))

    (scope/gat-ident? term-or-ident)
    (let [nm (:name term-or-ident)
          idx (.indexOf (mapv :name (:idents ctx)) nm)]
      (when (neg? idx)
        (throw (ex-info (str "Ident not bound in axiom context: " nm)
                        {:ident nm :ctx-names (mapv :name (:idents ctx))})))
      (alg-type->form (nth (:types ctx) idx)))

    :else
    (throw (ex-info "Cannot determine sort for term form" {:got term-or-ident}))))

(defn- axiom->field-form
  "Axiom → s-expr field type for the inductive's mk constructor.

   - assoc : ∀ x y z : El, Eq El ((x*y)*z) (x*(y*z))
       → (forall [x El, y El, z El]
           (= El (mul (mul x y) z) (mul x (mul y z))))

   Uses 3-arg `(= ty lhs rhs)` so compile-type does not default to Nat.
   The sort is resolved from the LHS (or RHS if LHS is a bare ident)
   via term-sort-form — katzen's parser leaves the AlgTerm :type field
   as a placeholder."
  [gat axiom]
  (let [ctx       (:ctx axiom)
        binders   (ctx->binders ctx)
        ;; Prefer LHS for sort inference; fall back to RHS if LHS is a
        ;; bare ident whose ctx-binding we'd already have computed.
        sort-form (term-sort-form gat ctx
                                  (if (core/alg-term? (:lhs axiom))
                                    (:lhs axiom)
                                    (:rhs axiom)))
        lhs-form  (alg-term->form (:lhs axiom))
        rhs-form  (alg-term->form (:rhs axiom))
        eq-form   (list '= sort-form lhs-form rhs-form)]
    (if (empty? binders)
      eq-form
      (list 'forall binders eq-form))))

(defn- gat->params-spec
  "GAT → flat params vector for ansatz/define-inductive."
  [gat]
  (vec (mapcat sort->param-spec (:type-constructors gat))))

(defn- gat->ctors-spec
  "GAT → single-constructor ctors-spec for ansatz/define-inductive.

   Fields are emitted in this order:
     1. Term constructors (mul, unit, compose, id, …)
     2. Axiom witnesses (assoc, unit-left, …)

   The kernel's telescope discipline lets later fields reference earlier
   ones, so axiom field types can reference the term constructors above
   them. The structure is admitted with :no-confusion? false to bypass
   Bug C — see check-theory! for context."
  [gat]
  (let [term-pairs (map (fn [tc]
                          [(-> tc :term :head :name)
                           (term->field-form tc)])
                        (:term-constructors gat))
        axiom-pairs (map (fn [ax]
                           [(:name ax) (axiom->field-form gat ax)])
                         (:axioms gat))
        fields (vec (mapcat identity (concat term-pairs axiom-pairs)))]
    [['mk fields]]))

(defn- struct-name
  "Convention: ThMonoid → ThMonoidStr. Keeps katzen GAT name distinct
   from its CIC encoding."
  [gat]
  (str (:name gat) "Str"))

;; -------------------------------------------------------------------
;; Public API
;; -------------------------------------------------------------------

(defn- with-installed-theory*
  "Install gat's CIC encoding, run f with the temporarily-augmented env,
   then restore. f receives [env struct-const-info]."
  [gat f]
  (let [ansatz-env-atom @(requiring-resolve 'ansatz.core/ansatz-env)
        original-env (a/env)
        params-spec  (gat->params-spec gat)
        ctors-spec   (gat->ctors-spec gat)
        ind-name-str (struct-name gat)]
    (try
      (binding [a/*verbose* false]
        (a-ind/define-inductive original-env ind-name-str
                                params-spec ctors-spec
                                :no-confusion? false))
      (let [env (a/env)
            struct-ci (a-env/lookup env (a-name/from-string ind-name-str))]
        (f env struct-ci))
      (finally
        (reset! ansatz-env-atom original-env)))))

(defn check-theory!
  "Type-check a katzen GAT against the Lean 4 CIC kernel embedded in
   ansatz. Returns :ok on success, throws ex-info on kernel error.

   Non-destructive: the ansatz global env is restored after the check,
   so theories can be checked independently in any order.

   Requires (a/init! ...) to have been called beforehand to load a CIC
   environment (Mathlib, cslib, or minimal init)."
  [gat]
  (when-not (core/gat? gat)
    (throw (ex-info "check-theory! expects a katzen GAT value" {:got gat})))
  (try
    (with-installed-theory* gat (fn [_env _ci] :ok))
    (catch Exception e
      (throw (ex-info (str "Theory failed CIC check: " (:name gat)
                           " — " (.getMessage e))
                      {:theory (:name gat)} e)))))

(defn- sort-binding-form
  "Compile a sort-binding s-expr into a kernel Expr in the given env.

   Routes through ansatz.surface.term/term, which accepts the full
   surface vocabulary: bare symbols, `(-> A B)`, `(forall …)`, `(lam …)`,
   `(Sort …)`, applications, and so on. Dependent-sort bindings like
   `(lam [a Nat, b Nat] Nat)` for a discrete category's Hom go through
   here."
  [env form]
  (a-term/term env form))

(defn check-instance!
  "Verify that a sort-bindings map produces a well-typed CIC structure
   for the given theory. Returns :ok or throws.

   sort-bindings: map from sort name (symbol) to a Lean form. For
   non-dependent sorts (ThMonoid's `El`), a bare symbol like `Nat`. For
   dependent sorts (ThCategory's `Hom : Ob → Ob → Type`), a lambda such
   as `(lam [a Ob, b Ob] Nat)` or any expression of type `Ob → Ob → Type`.

   Example:
     (check-instance! std/ThMonoid '{El Nat})
     ;; ⇒ :ok  (ThMonoidStr Nat is a valid Lean type)

   This is a SHAPE check: it confirms the sort signatures line up, but
   does not require proofs of the axioms. Verifying axiom witnesses
   would require expressing the model's Clojure implementation in CIC,
   which is out of scope for this layer."
  [gat sort-bindings]
  (when-not (core/gat? gat)
    (throw (ex-info "check-instance! expects a katzen GAT value" {:got gat})))
  (when-not (map? sort-bindings)
    (throw (ex-info "sort-bindings must be a map of sort-symbol → form"
                    {:got sort-bindings})))
  (try
    (with-installed-theory*
      gat
      (fn [env struct-ci]
        (let [;; Build (StructName binding1 binding2 ...) as a kernel app.
              struct-const (a-expr/const' (a-name/from-string (struct-name gat))
                                          (vec (.levelParams struct-ci)))
              sort-names (mapv type-ctor-name (:type-constructors gat))
              missing (remove sort-bindings sort-names)
              _ (when (seq missing)
                  (throw (ex-info (str "Missing sort bindings for: " (vec missing))
                                  {:missing missing
                                   :required sort-names
                                   :given (keys sort-bindings)})))
              arg-exprs (mapv (fn [s]
                                (sort-binding-form env (get sort-bindings s)))
                              sort-names)
              applied (reduce a-expr/app struct-const arg-exprs)
              st (a-tc/mk-tc-state (a/env))
              ;; Infer the type of the applied term. If it succeeds and
              ;; reduces to a Sort, the bindings are compatible.
              ty (a-tc/infer-type st applied)]
          ;; Sanity: the result should be a Sort.
          (when-not (a-expr/sort? ty)
            (throw (ex-info "Applied structure does not reduce to a Sort"
                            {:theory (:name gat)
                             :inferred-type (str ty)})))
          :ok)))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Exception e
      (throw (ex-info (str "Instance failed CIC check: " (:name gat)
                           " — " (.getMessage e))
                      {:theory (:name gat) :bindings sort-bindings}
                      e)))))

;; -------------------------------------------------------------------
;; check-morphism! — functoriality of a theory morphism
;; -------------------------------------------------------------------
;;
;; A theory morphism F : ThA → ThB is a functor: it maps sorts and
;; terms of ThA into ThB such that
;;
;;   (i)  Well-typedness — every dom sort is mapped to a codom sort,
;;        every dom term ctor's image has the right type in codom.
;;   (ii) Identity preservation — F(id) = id. Trivial when both theories
;;        present id as the same generator.
;;   (iii) Composition preservation — F(g ∘ f) = F(g) ∘ F(f). Forced
;;        when ThA is given by generators and relations and F is
;;        defined on those.
;;   (iv) Equation preservation — for every axiom (LHS = RHS) of ThA,
;;        the image (F(LHS) = F(RHS)) must hold in ThB.
;;
;; (i) is already covered by katzen.morphism/validate-theory-morphism
;; (the wellformedness checks migrated from katzen.validation in
;; phase 1). (ii) and (iii) come for free for our shape of morphism:
;; we map generators to generators, so equations between generators
;; (the only kind of equation in a GAT) survive structurally.
;;
;; (iv) is the genuinely non-trivial check. For each ThA axiom
;; ∀<ctx>, LHS = RHS we must verify (∀<F(ctx)>, F(LHS) = F(RHS)) is
;; a theorem of ThB's CIC encoding. The strongest form of "verify"
;; is "the kernel accepts the proof"; the weakest is "the user has
;; tagged the obligation with :sorry."
;;
;; v1 covers the trivial-axioms case completely (ThA has no axioms,
;; so (iv) is vacuous). The infrastructure is in place; richer
;; morphism shapes plug in via the per-axiom proof-term registry.

(defn- term-image
  "Apply a morphism to an AlgTerm. Term constructors in the dom theory
   are replaced by their codomain image (via term-map); context idents
   (the bound vars of the axiom) are left in place — they live in the
   ambient ∀ of the obligation we're about to compile."
  [morphism alg-term]
  (let [{:keys [type-map term-map]} morphism]
    (cond
      (scope/gat-ident? alg-term)
      ;; Bound context var. The morphism may also retag a sort ident if
      ;; that ident plays a sort role; otherwise pass through.
      (get type-map alg-term alg-term)

      (core/alg-term? alg-term)
      (let [head (:head alg-term)
            args (:args alg-term)
            mapped-head (get term-map head head)
            mapped-args (mapv #(term-image morphism %) args)]
        (cond
          (core/alg-term? mapped-head)
          ;; Term map points to another AlgTerm (TermInCtx wrapper). For
          ;; the v1 surface where most mappings are generator→generator,
          ;; this branch is exercised by morphisms like
          ;;   compose(f,g) ↦ compose(g,f).
          (core/alg-term (:head mapped-head)
                         (mapv #(term-image morphism %)
                               (:args mapped-head))
                         (:type mapped-head))

          (scope/gat-ident? mapped-head)
          (core/alg-term mapped-head mapped-args (:type alg-term))

          :else
          alg-term))

      :else alg-term)))

(defn- axiom-obligation-form
  "Build the proof-obligation s-expr for one dom-theory axiom under a
   morphism. Returns the same shape we feed into compile-type elsewhere:

     (forall [<ctx-binders>] (= <sort> <lhs-image> <rhs-image>))

   The sort is the lhs's sort in the codomain — we resolve it via
   katzen.ansatz.export/term-sort-form already in this ns, which
   handles dependent return types by substituting the actual args."
  [codom-gat axiom morphism]
  (let [{:keys [ctx lhs rhs]} axiom
        binders (ctx->binders ctx)
        ;; Apply the morphism to LHS / RHS in the dom theory.
        lhs* (term-image morphism lhs)
        rhs* (term-image morphism rhs)
        ;; Determine the equation's sort under the morphism. The codom
        ;; theory's term constructors are the ground truth here.
        sort-form (term-sort-form codom-gat ctx
                                  (if (core/alg-term? lhs*) lhs* rhs*))
        lhs-form  (alg-term->form lhs*)
        rhs-form  (alg-term->form rhs*)
        eq-form   (list '= sort-form lhs-form rhs-form)]
    (if (empty? binders)
      eq-form
      (list 'forall binders eq-form))))

(defn- alg-term-structural=
  "Structural equality on AlgTerms / Idents / AlgTypes that ignores the
   scope-tag UUIDs — two equations from different scopes are considered
   equal if their *shapes* match: same head name, same arg names
   pointwise. This is the right notion for the trivial proof-search:
   if an axiom of the dom, after applying the morphism, is the same
   equation already declared in the codom, then it holds trivially
   (the codom's structure provides the proof field directly)."
  [a b]
  (cond
    (and (core/alg-term? a) (core/alg-term? b))
    (and (= (-> a :head :name) (-> b :head :name))
         (= (count (:args a)) (count (:args b)))
         (every? true? (map alg-term-structural= (:args a) (:args b))))

    (and (core/alg-type? a) (core/alg-type? b))
    (and (= (-> a :head :name) (-> b :head :name))
         (= (count (:args a)) (count (:args b)))
         (every? true? (map alg-term-structural= (:args a) (:args b))))

    (and (scope/gat-ident? a) (scope/gat-ident? b))
    (= (:name a) (:name b))

    :else (= a b)))

(defn- ctx-structural=
  "Structural equality on TypeCtx values — same idents (by name) and
   same types (structurally)."
  [a b]
  (and (= (mapv :name (:idents a)) (mapv :name (:idents b)))
       (every? true? (map alg-term-structural= (:types a) (:types b)))))

(defn- find-matching-codom-axiom
  "Look up a codomain axiom whose ctx, LHS, and RHS structurally match
   the morphism's image of `dom-ax`. Returns the matching codom axiom
   or nil. This is the trivial-proof case: the image lives in the
   codomain already, so its field provides the witness for free."
  [codom-gat morph dom-ax]
  (let [{:keys [ctx lhs rhs]} dom-ax
        lhs* (term-image morph lhs)
        rhs* (term-image morph rhs)]
    (first
     (for [cand (:axioms codom-gat)
           :when (and (ctx-structural=        ctx        (:ctx cand))
                      (alg-term-structural=   lhs*       (:lhs cand))
                      (alg-term-structural=   rhs*       (:rhs cand)))]
       cand))))

(defn- attempt-rfl
  "Verify a proof obligation by definitional equality (`rfl`). Returns
   true if the morphism's image equates LHS to RHS up to β/ι/η
   reduction in the codomain's encoding — this handles cases where the
   morphism rewrites a compound term back into the same shape."
  [env obligation-form]
  (try
    (let [obligation-expr (a-term/term env obligation-form)
          st              (a-tc/mk-tc-state env)
          _               (a-tc/infer-type st obligation-expr)
          ;; Walk under the foralls; what's left is `(Eq ty a b)`. We
          ;; let the kernel's def-eq machinery treat the bound vars
          ;; as opaque locals.
          inner (loop [e obligation-expr]
                  (if (a-expr/forall? e)
                    (recur (a-expr/forall-body e))
                    e))
          [lhs rhs] (when (a-expr/app? inner)
                      (let [rhs (.o1 inner)
                            inner2 (.o0 inner)]
                        (when (a-expr/app? inner2)
                          [(.o1 inner2) rhs])))]
      (boolean
       (when (and lhs rhs)
         (a-tc/is-def-eq st lhs rhs))))
    (catch Exception _ false)))

(defn- discharge-obligation
  "Try every proof-search tactic in order. Returns a status keyword
   describing how the obligation closed, or :unresolved.

   Tactics (in order):
     :codom-axiom-match  — the image already lives in codom as an axiom.
     :rfl                — definitional equality after reduction."
  [env codom-gat morph dom-ax obligation-form]
  (cond
    (find-matching-codom-axiom codom-gat morph dom-ax)
    :codom-axiom-match

    (attempt-rfl env obligation-form)
    :rfl

    :else :unresolved))

(defn check-morphism!
  "Verify a TheoryMorphism is a CIC-functoriality-witnessed map between
   two katzen theories.

   Workflow:
     1. Validate the morphism's well-typedness (sorts and terms point
        at real codom structures, contexts have matching arities).
     2. Kernel-check both theories (transitively re-runs check-theory!).
     3. For each axiom of the domain theory, build the corresponding
        proof obligation in the codomain and attempt to discharge it.

   By default the discharge attempt is :rfl (definitional equality —
   the most common case for migration-style morphisms where the dom is
   essentially a sub-presentation of the codom). Future versions can
   accept a per-axiom :proof-term map for cases that need real proofs.

   Modes (the :verify? option):
     :strict (default) — every obligation must close. Throws ex-info
                         on the first unresolved axiom.
     :informational    — same checks, but unresolved obligations log
                         a warning and the function returns
                         {:status :ok-with-sorry :unresolved [...]}.

   Returns :ok in strict mode when all obligations close; or the
   informational map when relaxed."
  ([morph] (check-morphism! morph {}))
  ([morph {:keys [verify?] :or {verify? :strict}}]
   (when-not (morphism/theory-morphism? morph)
     (throw (ex-info "check-morphism! expects a TheoryMorphism value" {:got morph})))
   (let [{:keys [dom codom]} morph]
     ;; Step 1 — wellformedness of the mapping itself.
     (morphism/validate-theory-morphism morph)
     ;; Step 2 — both theories are kernel-checked. We do them
     ;; independently in non-destructive mode, so the global env is
     ;; restored between calls.
     (check-theory! dom)
     (check-theory! codom)
     ;; Step 3 — for each dom axiom, build the obligation under the
     ;; morphism and attempt to discharge it in the codom's installed
     ;; CIC encoding.
     (let [ansatz-env-atom @(requiring-resolve 'ansatz.core/ansatz-env)
           original-env (a/env)
           unresolved
           (try
             (binding [a/*verbose* false]
               (a-ind/define-inductive original-env (struct-name codom)
                                       (gat->params-spec codom)
                                       (gat->ctors-spec codom)
                                       :no-confusion? false))
             (let [env (a/env)]
               (vec
                (for [ax (:axioms dom)
                      :let [obligation (axiom-obligation-form codom ax morph)
                            status     (discharge-obligation env codom morph ax obligation)]
                      :when (= status :unresolved)]
                  {:axiom (:name ax) :obligation obligation})))
             (finally
               (reset! ansatz-env-atom original-env)))]
       (cond
         (empty? unresolved)
         :ok

         (= verify? :informational)
         (do (binding [*out* *err*]
               (println "[katzen/ansatz] check-morphism! informational mode —"
                        (count unresolved) "axiom(s) of" (:name dom)
                        "not discharged under" (:name morph) ":"
                        (mapv :axiom unresolved)))
             {:status :ok-with-sorry :unresolved unresolved})

         :else
         (throw (ex-info (str "Morphism " (:name morph) " failed strict functoriality check: "
                              (count unresolved) " unresolved axiom(s) of " (:name dom))
                         {:morphism (:name morph)
                          :dom (:name dom)
                          :codom (:name codom)
                          :unresolved unresolved})))))))
