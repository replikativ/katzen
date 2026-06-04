# Katzen

[![Clojars Project](https://img.shields.io/clojars/v/org.replikativ/katzen.svg)](https://clojars.org/org.replikativ/katzen)
[![CircleCI](https://circleci.com/gh/replikativ/katzen.svg?style=shield)](https://circleci.com/gh/replikativ/katzen)
[![Slack](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C09622F337D)
[![Docs](https://img.shields.io/badge/docs-clay_notebooks-blue.svg)](https://replikativ.github.io/katzen/)
[![Last Commit](https://img.shields.io/github/last-commit/replikativ/katzen/main.svg)](https://github.com/replikativ/katzen/commits/main)

**Describe the structure of your data and processes once — then check, migrate,
relate, simulate, and draw it.**

Katzen is a Clojure port of the [AlgebraicJulia](https://github.com/AlgebraicJulia)
stack ([Catlab](https://github.com/AlgebraicJulia/Catlab.jl),
[GATlab](https://github.com/AlgebraicJulia/GATlab.jl),
[AlgebraicDynamics](https://github.com/AlgebraicJulia/AlgebraicDynamics.jl),
[AlgebraicPetri](https://github.com/AlgebraicJulia/AlgebraicPetri.jl)). Category
theory does the work under the hood; you mostly write schemas and ordinary
Clojure. The name nods to *Catlab* — *Katzen* is German for cats.

**▶ [Try the playground](https://replikativ.github.io/katzen/playground/)** — paste
a Clojure function and watch it become a string diagram, right in the browser, no
install.

## What you can do with it

- 🧱 **[Structured data with built-in invariants](doc/schemata.md)** — declare
  rules like "debits equal credits" or "no link dangles" *as part of the schema*;
  they're checked, not scattered across `assert`s.
- 🔀 **[Schema evolution without migration scripts](doc/schemata.md)** — a mapping
  between two schemas moves the data structure-preservingly, instead of an ad-hoc
  rewrite that silently drops things.
- 🔗 **Relate data of different shapes** — pattern-match across stores ("does this
  shape occur in my graph?") or join by a shared identity, with no bespoke join
  code.
- 🌀 **[Model & simulate dynamical systems](doc/composition.md)** — ODEs, Petri
  nets, reaction networks (SIR, predator–prey, kinetics), and control loops;
  compose subsystems with wiring diagrams and solve fast.
- 🗣️ **One little language, many interpreters** — define a small DSL once, then run
  it through many interpretations: evaluate, pretty-print, cost, compile, or draw.
- 🖼️ **[See code as string diagrams](doc/programs-as-diagrams.md)** — a function
  body becomes a dataflow picture (reuse = a fork, an unused binding = a dropped
  wire); explore it in the [playground](https://replikativ.github.io/katzen/playground/).
- ✅ **Prove a translation correct** — an optional [Lean](https://github.com/replikativ/ansatz)
  kernel can check that a schema/theory mapping really preserves the rules — a
  proof, not a test.
- 🔢 **Compile to fast numerics** — a simulation's right-hand side compiles to
  primitive JVM code (via [raster](https://github.com/replikativ/raster)) and
  solves with real ODE solvers.

**When to reach for it:** you have data or processes with real *structure* —
relations, invariants, several representations that must stay in sync — and you
want to declare it once and then have it *checked, migrated, related, and
simulated* for you. A worked treatment (knowledge, code, and accounting side by
side) is in [doc/schemata.md](doc/schemata.md).

## A 60-second taste

Schema evolution without a migration script. A schema *mapping* says how an old
shape relates to a new one; the data moves along it, structure-preserving:

```clojure
(require '[katzen.acset.migration :as m]
         '[katzen.acset.graphs :as gg]
         '[katzen.acset :as acset])

;; a weighted graph: 3 vertices, 2 weighted edges
(def W (let [[g _] (acset/add-parts (gg/weighted-graph) :V 3)
             [g _] (gg/add-weighted-edge g 1 2 5.0)
             [g _] (gg/add-weighted-edge g 2 3 7.0)] g))

;; migrate along `ForgetWeight` — drop the weight column, keep the graph
(def G (m/migrate gg/ForgetWeight W))
;; G is a plain digraph; the weights are structurally gone, the edges intact.
```

No loop wrote that migration — the schema mapping did. The same idea powers
relating stores, aggregating reports, and turning a process model into an ODE.

## For Clojurians coming from outside category theory

You don't need the theory to use katzen; familiar tools are good intuition:

- If you've used **`clojure.spec` or Datomic schema**, a *theory* (a GAT) is the
  same idea one level up: it specifies the operations valid values must support,
  not just their shape. An **ACSet** is a typed relational store with a built-in
  contract — close to a datalog db, but with schema *and* migration baked in.
- If you've used **core.async or transducers**, the wiring-diagram composition is
  the categorical version of a topology of channels: each box is a process,
  junctions identify channels, and one operation (`oapply`) substitutes the
  sub-systems into the outer pattern.

The concept bridge, with the analogies spelled out, is in
[doc/for-clojurians.md](doc/for-clojurians.md).

## Dig deeper

| Read | What you'll get |
|---|---|
| [15-minute tutorial](doc/tutorial.md) | from zero to a checked schema and a migration |
| [Concepts for Clojurians](doc/for-clojurians.md) | datalog / spec / core.async analogies |
| [Schemata](doc/schemata.md) | structured data + invariants, worked across knowledge / code / accounting |
| [Composition](doc/composition.md) | dynamical systems, Petri nets, control loops, and why naive composition fails |
| [Programs as diagrams](doc/programs-as-diagrams.md) | the code → string-diagram functor and its grounding |
| [Conventions](doc/CONVENTIONS.md) | naming, the `Sch*`/`Th*` split, backend transparency |
| [Architecture](doc/architecture.md) | the core + backends, and the Catlab/GATlab equivalence map |
| [Comparison notebook](dev/notebooks/comparison_with_catlab.clj) | a runnable side-by-side with the Julia stack |

## Install & try it

```clojure
;; deps.edn
org.replikativ/katzen {:mvn/version "RELEASE"}
```

The base library needs no extra dependencies:

```bash
git clone https://github.com/replikativ/katzen.git
cd katzen
clojure -X:test          # 505 tests, zero opt-in deps
```

Then run the runnable comparison-with-Julia notebook (it gracefully skips any
section whose optional alias is absent):

```bash
clojure -M:dev -m notebooks.comparison-with-catlab
```

## Optional capabilities

The base path needs nothing extra. Three optional aliases each unlock a major
capability:

- **`:datahike`** — a [Datahike](https://github.com/replikativ/datahike)-backed,
  datalog-queryable persistent store for your ACSets. `clojure -X:test-datahike`
- **`:raster`** — compile a simulation's right-hand side to primitive numerics and
  solve with real ODE solvers (Tsit5, DP5, …). `clojure -X:test-raster`
- **`:ansatz`** — discharge axiom / morphism obligations with a
  [Lean](https://github.com/replikativ/ansatz) kernel (verified translations).
  `clojure -X:test-ansatz`

## How it compares to `cats`

[funcool/cats](https://github.com/funcool/cats) gives you monads, functors, and
applicatives as everyday FP plumbing *inside* Clojure. Katzen sits on a different
axis: it treats whole categories as first-class data — theories, typed relational
stores, wiring diagrams — to reason about *whole languages and data shapes*
compositionally, with optional kernel-checked verification. They complement rather
than compete.

## License

MIT, matching [Catlab.jl](https://github.com/AlgebraicJulia/Catlab.jl/blob/main/LICENSE)
and [GATlab.jl](https://github.com/AlgebraicJulia/GATlab.jl/blob/main/LICENSE) so
cross-pollination is unencumbered. See [LICENSE](LICENSE).
