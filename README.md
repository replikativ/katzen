# Katzen

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
