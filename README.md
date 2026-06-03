# Katzen

[![Clojars Project](https://img.shields.io/clojars/v/org.replikativ/katzen.svg)](https://clojars.org/org.replikativ/katzen)
[![CircleCI](https://circleci.com/gh/replikativ/katzen.svg?style=shield)](https://circleci.com/gh/replikativ/katzen)
[![Slack](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C09622F337D)
[![Docs](https://img.shields.io/badge/docs-clay_notebooks-blue.svg)](https://replikativ.github.io/katzen/)
[![Last Commit](https://img.shields.io/github/last-commit/replikativ/katzen/main.svg)](https://github.com/replikativ/katzen/commits/main)

**Categorical programming for Clojure. Verified by Lean. Compiled to native numerics.**

Katzen is a Clojure port and evolution of the
[AlgebraicJulia](https://github.com/AlgebraicJulia) toolkit —
[Catlab.jl](https://github.com/AlgebraicJulia/Catlab.jl),
[GATlab.jl](https://github.com/AlgebraicJulia/GATlab.jl),
[AlgebraicDynamics.jl](https://github.com/AlgebraicJulia/AlgebraicDynamics.jl),
[AlgebraicPetri.jl](https://github.com/AlgebraicJulia/AlgebraicPetri.jl) —
with an additional path through the
[ansatz](https://github.com/replikativ/ansatz) Lean kernel for verified
theory morphisms and through [raster](https://github.com/replikativ/raster)
for typed primitive numerical code.

The name nods to *Catlab* — `Katzen` is German for cats. A cat lab, a
category lab, and an honest sibling to Catlab.jl.

**Quick links**: the [15-minute tutorial](doc/tutorial.md), the
[concept bridge for Clojurians](doc/for-clojurians.md) (datalog / spec
/ core.async analogies), the
[conventions document](doc/CONVENTIONS.md), and the
[Catlab/GATlab comparison notebook](dev/notebooks/comparison_with_catlab.clj)
(a runnable side-by-side with the Julia stack).

## What can I do with it?

Concretely, today:

- **Keep structured data in datahike and stop hand-writing validation.** Declare
  a schema — objects, foreign keys, typed columns — and its *invariants* as path
  equations ("debits equal credits", "no link dangles", "a task's next-state is
  reachable"). The kernel checks them (`check-axioms!`); your rules live in the
  schema, not scattered across `assert`s.
- **Migrate a schema without writing a migration script.** A schema morphism
  gives Δ-migration in one line — data moves structure-preservingly instead of
  being silently dropped by an ad-hoc rewrite.
- **Relate data of different shapes.** Find ACSet homomorphisms ("does this
  pattern occur in my graph?"), or cross-reference two stores by a shared
  identity (a URI) — join a ledger to a CRM to a wiki with no bespoke join code.
- **Aggregate with one fold.** Balances, counts, sums are a commutative-monoid
  rollup defined once — the same construction for every report.
- **Model and simulate processes.** Build a Petri net or reaction network (an
  SIR epidemic, enzyme kinetics, a queue, a population model) with mass-action /
  Hill / Michaelis–Menten rates; compose subsystems with wiring diagrams
  (`oapply`); compile to typed numerics and solve the ODEs fast (`:raster`).
- **Define a little language, get many interpreters.** A GAT is a DSL; one term
  runs through many algebras — evaluate, pretty-print, cost, compile, draw. (The
  repo even models *a category of Clojure programs*.)
- **Prove a translation is correct.** The `:ansatz` alias checks a schema/theory
  morphism with a Lean kernel — structure-preservation as a proof, not a test.

**When to reach for it:** you have data or processes with real *structure* —
relations, invariants, several representations that must stay in sync — and you
want it declared once and then *checked, migrated, related, and simulated* as
data. (If you just want `Functor`/`Monad` protocols for FP plumbing, that's
`cats`'s axis — see "What this gives you that `cats` doesn't" below.) A longer
treatment, with knowledge / code / accounting worked side-by-side, is in
[doc/schemata.md](doc/schemata.md).

### Composing dynamical systems is tricky — here's the principled way

Wiring subsystems together by hand goes wrong in subtle ways: shared variables
get duplicated, feedback is ill-defined, and the result isn't reusable. katzen
uses **operads of wiring diagrams** — a composition *pattern* says how parts
connect, and `oapply` (an operad algebra, i.e. a functor) computes the
composite, identifying shared variables correctly.

An SIR epidemic is the composite of two reaction networks that **share the
infected population** `I` (the same move as Lotka–Volterra sharing its prey):

```clojure
(require '[katzen.petri :as p] '[katzen.uwd :as uwd]
         '[katzen.uwd.dynamics :as ud] '[katzen.compile.core :as cc])

;; two primitive reaction networks
(defn infection [] ; S + I -> 2I   (species 1=S, 2=I)
  (let [n (p/petri) [n _](p/add-species n) [n _](p/add-species n) [n t](p/add-transition n)
        [n _](p/add-input n 1 t) [n _](p/add-input n 2 t)
        [n _](p/add-output n 2 t) [n _](p/add-output n 2 t)] n))
(defn recovery  [] ; I -> R        (species 1=I, 2=R)
  (let [n (p/petri) [n _](p/add-species n) [n _](p/add-species n) [n t](p/add-transition n)
        [n _](p/add-input n 1 t) [n _](p/add-output n 2 t)] n))

;; composition pattern: 3 junctions S,I,R; infection exposes [S I], recovery exposes [I R]
(let [d (uwd/uwd) [d js] (uwd/add-junctions d 3)
      [d B1 _] (uwd/add-box-with-ports d (take 2 js))
      [d B2 _] (uwd/add-box-with-ports d (drop 1 js))
      ;; oapply identifies the shared I, sums the dynamics → one composite system
      sir (ud/oapply d {B1 (ud/from-compilable (p/petri-dynamics (infection) {1 (/ 0.3 1000.0)}) [1 2])
                        B2 (ud/from-compilable (p/petri-dynamics (recovery)  {1 0.1})           [1 2])})
      rhs (cc/compile-clojure-rhs sir)]
  (cc/layout-of sir)                                    ; => 3 state classes: S, I, R (I merged, not duplicated)
  (last (:us (p/integrate-rk4 rhs [999.0 1.0 0.0] 0.0 100.0 0.5))))
;; => [60 4 936]   the classic epidemic curve, integrated from the composite
```

The composite is straight-line code (no diagram re-walk per step); add the
`:raster` alias to compile it to native numerics and solve with `Tsit5`.

Not every system is a clean mass-action net — textbook **Lotka–Volterra** has
independent birth/predation/death rates. For those, `katzen.ode/vector-field`
gives you a system from a symbolic field (still compiles to the fast raster
path), and `katzen.ode/raw-field` from an arbitrary Clojure closure (the
faithful analog of AlgebraicDynamics' raw-function resource sharer). Both
compose through the *same* `oapply` — predator–prey is growth + predation +
death **sharing the prey and predator populations**:

```clojure
(require '[katzen.ode :as ode] '[katzen.uwd :as uwd] '[katzen.petri :as p]
         '[katzen.uwd.dynamics :as ud] '[katzen.compile.core :as cc])

(let [growth    (ode/vector-field {:states '[r]   :params '{a 1.1} :field '{r (* a r)}})
      predation (ode/vector-field {:states '[r f] :params '{b 0.4 d 0.1}
                                   :field  '{r (- (* b r f)) f (* d r f)}})
      death     (ode/vector-field {:states '[f]   :params '{g 0.4} :field '{f (- (* g f))}})
      d (uwd/uwd) [d [jR jF]] (uwd/add-junctions d 2)
      [d Bg _] (uwd/add-box-with-ports d [jR])      ; growth touches the prey R
      [d Bp _] (uwd/add-box-with-ports d [jR jF])   ; predation couples R and F
      [d Bd _] (uwd/add-box-with-ports d [jF])      ; death touches the predator F
      lv  (ud/oapply d {Bg (ud/from-compilable growth    '[r])
                        Bp (ud/from-compilable predation '[r f])
                        Bd (ud/from-compilable death     '[f])})]
  (cc/layout-of lv)   ; => 2 state classes: R, F (prey/predator merged across the 3 boxes)
  (last (:us (p/integrate-rk4 (cc/compile-clojure-rhs lv) [10.0 10.0] 0.0 20.0 0.001))))
;; the composite conserves the LV first integral to ~1e-12 — a correct closed orbit
```

Directed composition (input/output **machines** with readouts) and **open Petri
nets → ODE** as a functor work the same way. See
[doc/composition.md](doc/composition.md) for the full story (and why naive
composition fails).

## Define a category

A *generalized algebraic theory* (GAT) is the data shape Katzen uses
to specify a category — types, term constructors, axioms. Here's
`ThCategory`, ported verbatim from
[GATlab.jl](https://github.com/AlgebraicJulia/GATlab.jl/blob/main/src/stdlib/theories/categories.jl):

```clojure
(require '[katzen.theory :refer [deftheory]])

(deftheory ThCategory
  (type Ob)                                     ; objects
  (type Hom [dom Ob, codom Ob])                 ; morphisms — dependent type
  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret  (Hom a c))                            ; composition
  (term id
    :ctx [a Ob]
    :ret  (Hom a a))                            ; identity
  (axiom assoc
    :ctx [a Ob, b Ob, c Ob, d Ob,
          f (Hom a b), g (Hom b c), h (Hom c d)]
    (= (compose (compose f g) h)
       (compose f (compose g h))))
  (axiom unit-left  :ctx [a Ob, b Ob, f (Hom a b)] (= (compose (id a) f) f))
  (axiom unit-right :ctx [a Ob, b Ob, f (Hom a b)] (= (compose f (id b)) f)))
```

The standard library (`katzen.stdlib.core`) ships `ThCategory`,
`ThMonoid`, `ThGroup`, `ThSchema`, `ThSymmetricMonoidalCategory`, and
`ThGraph`. The worked algebra hierarchy `ThSemigroup → ThCommutativeMonoid
→ ThGroup → ThAbelianGroup` lives in `katzen.library.algebra.*`.

A *model* of a theory is a Clojure implementation: `(definstance
NatAddMonoid ThMonoid ...)` says "natural numbers under + form a
monoid." A *symbolic model* is one that builds expression trees instead
of computing values: `(defsymbolic SymMonoid ThMonoid)`. Both satisfy
the same axioms — but one runs the math and the other constructs ASTs.

## What this gives you that `cats` doesn't

Clojure already has [funcool/cats](https://github.com/funcool/cats) —
a great library for monads, functors, and applicatives as everyday FP
abstractions. Katzen sits on a different axis:

| | `funcool/cats` | Katzen |
|---|---|---|
| Style | Type classes for FP idioms (monad, applicative, semigroup) | Whole categories as first-class data (theories, ACSets, wiring diagrams) |
| Focus | Compose functions and effects within Clojure | Reason about *whole languages* and *whole data shapes* compositionally |
| Translation between systems | Monad transformers | Theory / schema morphisms with kernel-verified axiom preservation |
| Composition of subsystems | `do` notation, applicative builders | Operadic `oapply` over UWDs / DWDs / CPGs |
| Verification | Convention — your monad satisfies the laws if you say so | Optional ansatz Lean kernel discharges axiom obligations |
| Numerical compile | n/a | `compile-rhs` → typed `raster.fn.IFn__doubles_doubles_double` |

The two libraries complement rather than compete. You can write a
monadic computation in `cats` whose carrier types come from Katzen
ACSets; you can use Katzen's UWD composition to plan a dataflow whose
boxes are `cats` monadic pipelines.

## A category of Clojure programs

The `katzen.examples.clojure-*` namespaces work an instructive example:
a GAT `ThClojureCore` defining a small Clojure-like language (literals,
variables, lambdas, application, `let`, `if`), together with two
models. `StandardEval` interprets each term constructor as eager call-by-
value evaluation. `SymbolicClojure` interprets each term constructor as
an AST node — same syntax, different semantics, both functorial.

This is exactly the kind of categorical thinking Catlab/GATlab make
practical: instead of writing an evaluator and a separate AST builder
that drift, you write the *theory* once and provide two *models*. The
fact that `SymbolicClojure` produces a `StandardEval`-interpretable
output is then a theory morphism, kernel-verifiable through ansatz.

See `src/katzen/examples/clojure_core.clj` for the theory and the
two models in `clojure_eval.clj` / `clojure_symbolic.clj`.

```clojure
(require '[katzen.petri :as p])
(require '[katzen.compile.core :as cc])

;; SIR Petri net: S + I → 2I (infection),  I → R (recovery)
(let [n (p/petri)
      [n s]   (p/add-species n)
      [n i]   (p/add-species n)
      [n r]   (p/add-species n)
      [n inf] (p/add-transition n)
      [n rec] (p/add-transition n)
      [n _]   (p/add-input  n s inf)
      [n _]   (p/add-input  n i inf)
      [n _]   (p/add-output n i inf)
      [n _]   (p/add-output n i inf)
      [n _]   (p/add-input  n i rec)
      [n _]   (p/add-output n r rec)

      ;; ACSet → typed ODE rhs → integrate
      dyn (p/petri-dynamics n {inf 0.0003 rec 0.1})
      rhs (cc/compile-clojure-rhs dyn)
      sol (p/integrate-rk4 rhs [999.0 1.0 0.0] 0.0 100.0 0.1)]
  (last (:us sol)))
;; => [60.34… 4.09… 935.57…]    ;; final S, I, R at t=100
```

Schema-driven, type-safe data migration:

```clojure
(require '[katzen.acset.migration :as m])
(require '[katzen.acset.graphs :as gg])

;; Drop edge weights — schema-morphism Δ-migration
(def W (let [[g _] (katzen.acset/add-parts (gg/weighted-graph) :V 3)
             [g _] (gg/add-weighted-edge g 1 2 5.0)
             [g _] (gg/add-weighted-edge g 2 3 7.0)] g))
(def G (m/migrate gg/ForgetWeight W))
;; G is a plain digraph; the weight column is structurally gone.
```

Symbolic normalization (zero deps; mirrors Catlab's `GATExprUtils.jl`):

```clojure
(require '[katzen.acset.normalize :as n])

(def SchMonoid
  {:objects [:El] :homs []
   :axioms [{:name 'assoc :ctx [{:name 'x :type :El}
                                {:name 'y :type :El}
                                {:name 'z :type :El}]
             :lhs '(mul (mul x y) z) :rhs '(mul x (mul y z))
             :canonical :lhs}
            {:name 'unit-left  :ctx [{:name 'x :type :El}]
             :lhs '(mul u x) :rhs 'x}
            {:name 'unit-right :ctx [{:name 'x :type :El}]
             :lhs '(mul x u) :rhs 'x}]})

(n/normalize SchMonoid '(mul a (mul b (mul c d))))
;; => (mul (mul (mul a b) c) d)
(n/normalize SchMonoid '(mul u (mul a (mul u b))))
;; => (mul a b)
```

## Try It

```bash
git clone https://github.com/replikativ/katzen.git
cd katzen
clojure -M:test                  # base suite — 505 tests, zero opt-in deps
```

Then run the comparison-with-Julia notebook (it's a runnable script, not a
Clay notebook; gracefully skips sections whose optional alias is absent):

```bash
clojure -M:dev -m notebooks.comparison-with-catlab
```

To run the full numerical and verification paths:

```bash
clojure -M:dev:raster:ansatz -m notebooks.comparison-with-catlab
```

## What you'll find

| Area | Namespace | Catlab/GATlab equivalent |
|---|---|---|
| Declare a theory | `katzen.theory` `deftheory`, `katzen.stdlib.core` | [GATlab.jl `@theory`](https://github.com/AlgebraicJulia/GATlab.jl/blob/main/src/syntax/gats/) |
| ACSets | `katzen.acset` `vector-acset`, `add-parts`, `set-subpart` | [Catlab.jl `@acset_type`](https://github.com/AlgebraicJulia/Catlab.jl/tree/main/src/categorical_algebra) |
| Schema morphisms + Δ-migration | `katzen.acset.migration` `schema-morphism`, `migrate`, `migrate-morphism` | [Catlab.jl `DeltaMigration`](https://github.com/AlgebraicJulia/Catlab.jl/blob/main/src/categorical_algebra/pointwise/FunctorialDataMigrations.jl) |
| ACSet morphisms + naturality | `katzen.acset.morphism` `acset-morphism`, `natural?`, `compose` | [Catlab.jl `is_natural`](https://github.com/AlgebraicJulia/Catlab.jl/blob/main/src/categorical_algebra/pointwise/csets/CSets.jl) |
| Homomorphism search | `katzen.acset.homomorphism` `homomorphisms`, `nhomomorphisms` | [Catlab.jl `homomorphisms`](https://github.com/AlgebraicJulia/Catlab.jl/blob/main/src/categorical_algebra/pointwise/_HomSearch.jl) |
| Instance axiom enforcement | `katzen.acset.check` `check-axioms`, `check-axioms!` | none — Catlab leaves this to user code |
| Symbolic normalization | `katzen.acset.normalize`, `katzen.symbolic.normalize` `normalize`, `equiv?` | [GATlab.jl `GATExprUtils`](https://github.com/AlgebraicJulia/GATlab.jl/blob/main/src/models/GATExprUtils.jl) |
| Lean-kernel verification | `katzen.ansatz.export`, `katzen.acset.theory-bridge` `check-theory!`, `verify-schema-morphism!`, `verified-migrate` | none — GATlab's `TheoryMaps.jl:256` has an open TODO |
| FinSet (co)limits | `katzen.finset.limits`, `katzen.finset.colimits` `product`, `pullback`, `coproduct`, `pushout` | [Catlab.jl `FinSet` limits](https://github.com/AlgebraicJulia/Catlab.jl/tree/main/src/categorical_algebra/setcats) |
| UWD composition | `katzen.uwd.dynamics` `uwd`, `oapply` | [AlgebraicDynamics.jl `uwd_dynam.jl`](https://github.com/AlgebraicJulia/AlgebraicDynamics.jl/blob/main/src/uwd_dynam.jl) |
| DWD composition | `katzen.dwd.dynamics` `dwd`, `oapply-dwd`, `machine` | [AlgebraicDynamics.jl `dwd_dynam.jl`](https://github.com/AlgebraicJulia/AlgebraicDynamics.jl/blob/main/src/dwd_dynam.jl) |
| Circular port graphs | `katzen.cpg` | [AlgebraicDynamics.jl `cpg_dynam.jl`](https://github.com/AlgebraicJulia/AlgebraicDynamics.jl/blob/main/src/cpg_dynam.jl) |
| Vector fields (non-net dynamics) | `katzen.ode` `vector-field` (symbolic), `raw-field` (closure) | [AlgebraicDynamics.jl `ContinuousResourceSharer`](https://github.com/AlgebraicJulia/AlgebraicDynamics.jl/blob/main/src/uwd_dynam.jl) |
| Petri nets | `katzen.petri` `petri`, `petri-dynamics`, `integrate-rk4`, `migrate-dynamics` | [AlgebraicPetri.jl](https://github.com/AlgebraicJulia/AlgebraicPetri.jl) |
| Reaction networks | `katzen.reaction` `reaction-network`, `reaction-dynamics` (mass-action, Michaelis-Menten, Hill, `:expr`) | [Catalyst.jl](https://github.com/SciML/Catalyst.jl) |
| Numerical compile | `katzen.compile.core` `RasterCompilable`, `compile-rhs`, `compile-clojure-rhs` | [AlgebraicPetri.jl `vectorfield_expr`](https://github.com/AlgebraicJulia/AlgebraicPetri.jl/blob/main/src/AlgebraicPetri.jl) (via `GeneralizedGenerated.mk_function`) |

## Optional aliases

The base `:test` and `:dev` paths require no extra dependencies. Three
optional aliases each unlock a substantial capability:

- **`:datahike`** — `io.replikativ/datahike`. Datalog-queryable persistent
  ACSet backend; unlocks `katzen.acset.datahike/DatahikeACSet` and the
  datalog-based homomorphism search. Run `clojure -X:test-datahike`.
- **`:raster`** — `org.replikativ/raster` + `--add-modules=jdk.incubator.vector`.
  Unlocks `katzen.compile.core/compile-rhs`, which returns a typed
  `raster.fn.IFn__doubles_doubles_double` that `raster.ode/solve`
  (Tsit5, DP5, Rosenbrock23, …) accepts directly. Run `clojure -X:test-raster`.
- **`:ansatz`** — `org.replikativ/ansatz` + `--enable-native-access=ALL-UNNAMED`.
  Unlocks `katzen.ansatz.export/check-morphism!` and
  `katzen.acset.theory-bridge/verify-schema-morphism!`. Requires a
  pre-built Mathlib store at `/var/tmp/ansatz-mathlib` (see
  [ansatz](https://github.com/replikativ/ansatz)). Run
  `clojure -X:test-ansatz`.

The `katzen.test-support` ns centralizes the availability predicates
`datahike-available?`, `raster-available?`, `ansatz-ready?`.

## For Clojurians coming from outside AlgebraicJulia

If you've used `clojure.spec` or core.typed, GATs will feel familiar: a
schema specifies the shape of valid values, and a theory specifies the
*operations* that values must support. An ACSet is essentially a typed
relational store with a categorical contract — close to a datalog db
in spirit, but with the schema and migration story baked in.

If you've used core.async or transducers, the UWD/DWD composition path
is the categorical analogue of a topology of channels: each box is a
process; junctions identify channels; oapply substitutes the
sub-systems into the outer pattern. The new piece is that the
*composition algebra* is parameterized — relations, counts, ODE
systems, Petri nets all plug into the same UWD topology.

See [`doc/CONVENTIONS.md`](doc/CONVENTIONS.md) for:

- The `Sch*` vs `Th*` split (`Sch*` = schema data map for ACSets;
  `Th*` = GAT theory presentation; bridge translates).
- The **accumulate-not-zero** contract for `RasterCompilable` bodies.
- Backend transparency for dynamics (Petri/Reaction work on vector and
  datahike ACSets through the same protocol).

## Test suites

```
clojure -X:test                 # 505/1702 — base, no optional deps
clojure -X:test-raster          # 47/138 — raster compile path
clojure -X:test-ansatz          # 20/26 — ansatz Lean kernel
clojure -X:test-datahike        # datahike backend
```

All four pass cleanly.

## License

MIT, matching [Catlab.jl](https://github.com/AlgebraicJulia/Catlab.jl/blob/main/LICENSE)
and [GATlab.jl](https://github.com/AlgebraicJulia/GATlab.jl/blob/main/LICENSE)
so cross-pollination is unencumbered. See [`LICENSE`](LICENSE).
