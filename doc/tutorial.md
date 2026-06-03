# katzen tutorial — a 15-minute walkthrough

This walkthrough takes you from "I've cloned the repo" to a working
verified migration + compiled ODE pipeline. No optional aliases needed
for parts 1 and 2; part 3 uses the in-house RK4 integrator and stays in
the base distribution; part 4 shows the ansatz Lean-kernel verification
path and requires the `:ansatz` alias plus a Mathlib store.

## Setup

```bash
git clone https://github.com/replikativ/katzen.git
cd katzen
clj                  # opens a REPL on the base path
```

Or run the comparison notebook to see all of this end-to-end:

```bash
clojure -M:dev -m notebooks.comparison-with-catlab
```

---

## Part 1 — A schema with axioms (5 min)

**Goal**: declare a schema for symmetric graphs (each edge has an inverse
partner) with the involution law `inv(inv(e)) = e`, build an instance,
catch a bad instance.

```clojure
(require '[katzen.acset :as a])
(require '[katzen.acset.graphs :as gg])
(require '[katzen.acset.check :as check])

;; Catlab's SchSymmetricGraph lives at
;;   Catlab.jl/src/graphs/BasicGraphs.jl:276
;; Their version declares the involution law but doesn't enforce it.
;; Ours does.

(def SchSymGraph
  (assoc gg/SchSymmetricGraph
         :name 'SchSymGraphAx
         :axioms [{:name 'inv-involution
                   :ctx [{:name 'e :type :E}]
                   :lhs '(inv (inv e))
                   :rhs 'e}]))

;; A symmetric graph with two vertices and one undirected edge (= two
;; directed edges with inv pointing at each other).
(def good
  (let [g (a/vector-acset SchSymGraph)
        [g _]      (a/add-parts g :V 2)
        [g [e1 e2]] (a/add-parts g :E 2)]
    (-> g
        (a/set-subpart :src e1 1) (a/set-subpart :tgt e1 2)
        (a/set-subpart :src e2 2) (a/set-subpart :tgt e2 1)
        (a/set-subpart :inv e1 e2) (a/set-subpart :inv e2 e1))))

(check/check-axioms good)
;; => nil          (every axiom holds on every binding)

;; Now break the invariant — add a stray edge e3 with inv pointing at e1.
(def bad
  (let [[g [e3]] (a/add-parts good :E 1)]
    (-> g (a/set-subpart :src e3 1)
          (a/set-subpart :tgt e3 1)
          (a/set-subpart :inv e3 1))))   ; inv(e3)=e1, so inv(inv(e3))=e2 ≠ e3

(check/check-axioms bad)
;; => {:axiom inv-involution :bindings {e 3}
;;     :lhs-eval 2 :rhs-eval 3}
```

What just happened: the schema declares a structural law, and
`check-axioms` evaluates it against every binding of the context
variables. Catlab and GATlab parse the same kind of declaration but
leave the enforcement to user code (a manually-written `add_edges!`
that maintains the invariant). We catch it at the data boundary.

---

## Part 2 — Migrate the instance through a schema morphism (5 min)

**Goal**: forget the symmetry. The result is a plain digraph whose edges
include both directions of each undirected pair.

```clojure
(require '[katzen.acset.migration :as m])

;; gg/ForgetSymmetric is a SchemaMorphism: SchGraph ↪ SchSymmetricGraph,
;; mapping V→V, E→E, src→src, tgt→tgt. Δ-migration along it forgets
;; the inv hom column.
;;
;; Catlab equivalent: DeltaMigration in
;;   Catlab.jl/src/categorical_algebra/pointwise/FunctorialDataMigrations.jl

(def G (m/migrate gg/ForgetSymmetric good))

(a/nparts G :V)     ;; => 2
(a/nparts G :E)     ;; => 2
(a/schema G)        ;; => the plain SchGraph

;; ACSet morphisms migrate too. Build a morphism between two symmetric
;; graphs, then migrate it to a morphism of plain digraphs.
(require '[katzen.acset.morphism :as am])

(def id-phi (am/identity-morphism good))
(am/natural? id-phi)                     ;; => true
(def id-phi-on-G (m/migrate-morphism gg/ForgetSymmetric id-phi))
(am/natural? id-phi-on-G)                ;; => true (functoriality of Δ)
```

What just happened: schema morphisms drive Δ-migration both for ACSet
instances (`migrate`) and for ACSet morphisms (`migrate-morphism`). The
categorical theorem says Δ is a functor, so naturality of the source
morphism is preserved.

---

## Part 3 — Compose dynamics via UWD, compile, integrate (5 min)

**Goal**: build a coupled SIR by composing an "infection" sub-Petri-net
with a "recovery" sub-Petri-net through an undirected wiring diagram,
then compile to a typed ODE rhs and integrate.

```clojure
(require '[katzen.petri :as p])
(require '[katzen.uwd :as uwd])
(require '[katzen.uwd.dynamics :as ud])
(require '[katzen.compile.core :as cc])

;; Infection: 2 species (S, I), 1 transition S+I → 2I.
(def infection
  (let [n (p/petri)
        [n _S] (p/add-species n)
        [n _I] (p/add-species n)
        [n t]  (p/add-transition n)
        [n _]  (p/add-input  n 1 t)
        [n _]  (p/add-input  n 2 t)
        [n _]  (p/add-output n 2 t)
        [n _]  (p/add-output n 2 t)]
    n))

;; Recovery: 2 species (I, R), 1 transition I → R.
(def recovery
  (let [n (p/petri)
        [n _I] (p/add-species n)
        [n _R] (p/add-species n)
        [n t]  (p/add-transition n)
        [n _]  (p/add-input  n 1 t)
        [n _]  (p/add-output n 2 t)]
    n))

;; UWD: 3 junctions (one per species), each box's ports wire to them.
;; AlgebraicDynamics equivalent:
;;   AlgebraicDynamics.jl/src/uwd_dynam.jl

(def sir-uwd
  (let [d (uwd/uwd)
        [d js]   (uwd/add-junctions d 3)
        [d B1 _] (uwd/add-box-with-ports d (take 2 js))   ; infection ports → S, I
        [d B2 _] (uwd/add-box-with-ports d (drop 1 js))]  ; recovery ports → I, R
    {:d d :B1 B1 :B2 B2}))

;; Wrap each Petri net as a continuous-resource-sharer, declaring which
;; species each port exposes.
(def crs-infection (ud/from-compilable
                    (p/petri-dynamics infection {1 0.0003}) [1 2]))
(def crs-recovery  (ud/from-compilable
                    (p/petri-dynamics recovery  {1 0.1})    [1 2]))

;; Compose: the result is itself a RasterCompilable on the 3-species
;; composite state (S, I, R as the coequalizer classes of per-box
;; species under junction identifications).
(def composite (ud/oapply (:d sir-uwd)
                          {(:B1 sir-uwd) crs-infection
                           (:B2 sir-uwd) crs-recovery}))

;; Compile to a vanilla Clojure rhs (zero deps) and integrate.
(def rhs (cc/compile-clojure-rhs composite))
(def sol (p/integrate-rk4 rhs [999.0 1.0 0.0] 0.0 100.0 0.1))

(last (:us sol))
;; => [60.34… 4.09… 935.57…]   ;; final S, I, R
```

What just happened: oapply over the UWD computes the FinSet coequalizer
of per-box species sets along junction identifications (so the two
`I`s — one from each box — become the same composite species). The
composite is itself a `RasterCompilable`; the compile driver emits the
mass-action ODE as a typed body.

For the **raster** path — which JIT-compiles the same body into typed
primitive bytecode and feeds it to Tsit5 — replace
`compile-clojure-rhs` with `compile-rhs` and add the `:raster` alias.
The compiled form integrates ~10× faster.

---

## Part 4 — Verify the morphism through the Lean kernel (optional)

**Goal**: prove that the involution axiom is preserved when migrating
through a schema morphism, by routing the morphism through ansatz's
CIC kernel.

Requires the `:ansatz` alias and a Mathlib store at
`/var/tmp/ansatz-mathlib`. See [ansatz](https://github.com/replikativ/ansatz)
for setup.

```clojure
(require '[ansatz.core :as ansatz])
(ansatz/init! "/var/tmp/ansatz-mathlib" "mathlib")

(require '[katzen.acset.theory-bridge :as tb])

;; Identity-on-axiomized-SchSymmetricGraph — the simplest morphism that
;; actually has an axiom obligation to discharge.
(def IdSym
  (m/schema-morphism 'IdSym SchSymGraph SchSymGraph
                     {:V :V :E :E}
                     {:src [:src] :tgt [:tgt] :inv [:inv]}))

(tb/verify-schema-morphism! IdSym)
;; → ✓ inductive SchSymGraphAxStr defined …
;; → :ok          (the involution obligation discharged via :codom-axiom-match)

;; verified-migrate combines the bridge check with check-axioms! on
;; input and output. The pipeline is now:
;;   1. F verified through ansatz to preserve axioms
;;   2. input X satisfies its schema's axioms
;;   3. migrate
;;   4. output Y satisfies the dom-schema's axioms
(tb/verified-migrate IdSym good)
;; → an SchSymGraph ACSet with all involution invariants intact
```

What just happened: we bridged the schema to a katzen GAT, the
SchemaMorphism to a TheoryMorphism, and let ansatz prove the axiom
preservation. The base GATlab.jl explicitly leaves this as a TODO
(`TheoryMaps.jl:256` reads *"axioms are not mapped to proofs. TODO."*).

---

## Part 5 — A category of Clojure programs (optional)

**Goal**: see one theory carrying *two* interpretations — call-by-value
evaluation and AST construction — over the same syntax. This is the
categorical-programming move: theory once, models many.

```clojure
;; The theory: a small Clojure-like language as a GAT.
(require '[katzen.examples.clojure-core :refer [ThClojureCore]])

;; Two models of that theory.
(require '[katzen.examples.clojure-eval :refer [StandardEval]])         ; runs code
(require '[katzen.examples.clojure-symbolic :refer [SymbolicClojure]])  ; builds ASTs

;; Same surface syntax, two different categories of "value":
;;
;;   StandardEval.lit-num    : Number → Value      (the number itself)
;;   StandardEval.lam        : Sym × Expr → Value  (a Clojure closure)
;;   StandardEval.app        : Value × Value → Value
;;
;;   SymbolicClojure.lit-num : Number → Expr       ({:head :lit-num :args [n]})
;;   SymbolicClojure.lam     : Sym × Expr → Expr   ({:head :lam :args [sym body]})
;;   SymbolicClojure.app     : Expr × Expr → Expr
;;
;; The functor witnessed by `eval-with` between SymbolicClojure and
;; StandardEval IS the "interpreter": given an AST built by the symbolic
;; model, produce the value the eval model would have produced directly.
```

What just happened: by writing the *theory* `ThClojureCore` once and
providing two *models* (`StandardEval`, `SymbolicClojure`), you get two
non-trivial functors into different target categories — `Set` (values)
for one, the syntactic category of ASTs for the other. No drift between
the evaluator and the AST builder; they share the theory's term
constructors as their interface.

This is the kind of categorical thinking the Catlab/GATlab tradition
makes practical. The Clojure library [funcool/cats](https://github.com/funcool/cats)
gives you monads, applicatives, and functors as *FP type classes for
working inside Clojure*. Katzen gives you *whole languages and whole
data shapes as composable categorical objects* — translation between
them happens via theory morphisms, not monad transformers.

See `src/katzen/examples/clojure_core.clj` for the theory and the two
models in `clojure_eval.clj` / `clojure_symbolic.clj`. For a complete
walkthrough of theory-morphisms-as-translations, the
[comparison notebook](../dev/notebooks/comparison_with_catlab.clj)
ties this back to migration and verification.

---

## Where to next

- **Compose more interesting dynamics**: `katzen.dwd.dynamics` for
  directed routing (Machines, inputs/outputs, hierarchical
  composition), `katzen.cpg` for circular port graphs.
- **Reaction networks** with non-mass-action rate laws: `katzen.reaction`
  ships `:mass-action`, `:michaelis-menten`, `:hill`, and an `:expr`
  escape hatch.
- **Symbolic normalization**: `katzen.acset.normalize` for schema-axiom
  terms, `katzen.symbolic.normalize` for typed AlgTerms. See
  [`comparison_with_catlab.clj`](../dev/notebooks/comparison_with_catlab.clj)
  for live examples.
- **GAT theories**: `katzen.theory/deftheory` and the standard library
  in `katzen.stdlib.core`. See
  [`doc/CONVENTIONS.md`](CONVENTIONS.md) for the `Sch*` vs `Th*` split.
- **Conventions**: [`doc/CONVENTIONS.md`](CONVENTIONS.md) covers the
  Sch/Th split, the accumulate-not-zero contract for
  `RasterCompilable` bodies, and the optional-alias structure.
- **Concept bridge** if categorical jargon is new:
  [`doc/for-clojurians.md`](for-clojurians.md).
