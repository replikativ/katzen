# Architecture: a pure core and three optional backends

katzen is a small **pure-Clojure core** — schemas as data, ACSets as functors,
composition as operad algebras — surrounded by **three optional backends**, each
behind a deps alias. Nothing in the core requires them; each lights up one extra
capability.

```
                    ┌───────────────────────────────┐
                    │   katzen core (pure Clojure)   │
                    │  schema-map · ACSet · oapply   │
                    │  equations · migration · xref  │
                    └───────────────────────────────┘
                       /            |            \
                      /             |             \
               datahike          raster          ansatz
              (storage)        (numerics)        (proof)
            ACSet backend     RasterCompilable   Th-morphism
            durable parts    → compile-rhs → ODE  → Lean kernel
              :datahike         :raster           :ansatz
```

The core works with no aliases at all (an ACSet over the in-memory vector
backend, composed, equation-checked, migrated, cross-referenced). Add an alias
only for what you need: persistence, fast numerics, or machine-checked proofs.

## 1. The core: schema as data, ACSet as functor

A **schema** is a plain map — objects, homs (foreign keys), attr-types (value
sorts), attrs (typed columns), and equations (path invariants). An **ACSet** on
that schema is a functor `C → Set`: the parts of each object, plus, for each
morphism, a function from domain parts to codomain (a part-id for a hom, a value
for an attr).

| concern | namespace | what it gives |
|---|---|---|
| ACSets | `katzen.acset` | `vector-acset`, `add-parts`, `subpart`, `incident`; the `PACSet` protocol; `merge-schema`, `rename-schema` |
| invariants | `katzen.acset.check` | `check-axioms!` — path equations (`:equations`) enforced as data |
| migration | `katzen.acset.migration` | `schema-morphism`, `migrate` — functorial Δ data migration |
| (co)limits | `katzen.finset.{limits,colimits}`, `katzen.acset.colimits` | `pullback`/`pushout`/…; ACSet `coproduct` & `pushout` (gluing) |
| references | `katzen.xref` | pullback over a shared `Identity` attr — cross-ACSet links |
| type-side | `katzen.eval` | term evaluation: computed properties, validation |
| aggregation | `katzen.aggregate` | monoid folds (sum / count / min / …): balances, rollups |

Because the schema is a *value*, you compose, rename, and verify it like any
data — the basis for the domain-integration story in §5.

## 2. Composition: one `oapply` over many operads

Wiring diagrams (UWD / DWD / CPG) are themselves ACSets; composing
box-systems through them is `oapply`. Catlab exposes one `oapply` overloaded by
multiple dispatch; katzen mirrors that with a single multimethod:

- **`katzen.compose/oapply`** dispatches on `[operad algebra]` — operad from the
  diagram's schema name (`SchUWD` / `SchDWD` / `SchCPG`), algebra from the
  box-system record type (resource sharer / machine). `(oapply d boxes)` routes
  regardless of operad.
- The per-operad laws live in `katzen.uwd.dynamics`, `katzen.dwd.dynamics`,
  `katzen.cpg` and remain callable directly. Set-/scalar-valued algebras
  (`uwd/oapply-relations`, `uwd.algebras/oapply-scalar`) and `petri/compose-petri`
  keep their own names — they carry algebra-specific extra arguments, so they are
  a different function *shape*, not the same call at another type.

Sources of a box's dynamics: `petri-dynamics` (mass-action nets),
`katzen.ode/vector-field` (symbolic), `katzen.ode/raw-field` (opaque closure) for
the undirected side; `katzen.dwd.dynamics/raw-machine` and `vector-machine` for
the directed (control) side. See [composition.md](composition.md).

## 3. The numerics backend: raster (`:raster`)

The compile path is itself a **protocol**, not a pile of per-type functions —
the same open-extension shape as `oapply`. `katzen.compile.core/RasterCompilable`
has three methods:

1. `state-layout` — the state-vector shape (`:size`, and `:index-of` mapping
   natural labels → integer slots),
2. `raster-body [x layout]` — a *sequence of source forms* that **accumulate**
   into `du`,
3. `clojure-body [x layout]` — the same as a vanilla `(fn [du u t])`.

Two generic drivers:

- **`compile-rhs`** evals the raster body through `raster.core/ftm` (raster's
  typed-reify macro — katzen's analog of Julia's `GeneralizedGenerated.mk_function`)
  into a typed `raster.fn.IFn__doubles_doubles_double` that `raster.ode/solve`
  consumes directly (Tsit5, DP5, Rosenbrock23, …).
- **`compile-clojure-rhs`** returns the vanilla Clojure fn — no raster dep; for
  debugging, one-shot evaluation, or autograd.

The **accumulate-not-zero contract** (the body accumulates; the driver zeroes
`du` once at the top) is what makes operadic composition free: when two boxes
share a junction their states land in the same global slot and the contributions
sum automatically. Everything implementing the protocol —
`petri-dynamics`, `vector-field`, `raw-field`, closed `Machine`s, and every
`oapply` composite — plugs into both drivers. **Adding a new concept needs no
changes to `compile.core`.**

> Footgun: `compile-rhs` emits qualified `raster.*` forms that `eval` must
> resolve, so the *caller* must have the raster namespaces loaded (i.e. the
> `:raster` alias active). The Clojure path needs nothing.

## 4. The proof backend: ansatz (`:ansatz`)

Orthogonal to numerics. Where raster makes a model *run fast*, ansatz proves a
*translation is correct*. `katzen.ansatz.export/check-morphism!` and the
`katzen.acset.theory-bridge` (`check-theory!`, `verify-schema-morphism!`,
`verified-migrate`) discharge a theory- or schema-morphism's axiom obligations to
a **Lean kernel** — structure-preservation as a *proof*, not a test (requires a
prebuilt Mathlib store).

This is the reason for the **`Sch*` / `Th*` split** (see
[CONVENTIONS.md](CONVENTIONS.md)): a `Sch*` schema carries only what the ACSet
machinery needs; a `Th*` theory carries the extra structure ansatz wants. GATlab's
`TheoryMaps.jl` has an open TODO for exactly this kernel verification — katzen
doing it is a differentiator.

## 5. Domains: machinery here, schemas in your app

> **katzen ships the machinery and a generic vocabulary. A domain schema is a
> *presentation that uses* katzen, and lives in the app that owns the domain.**

`katzen.schema.*` carries only general shapes (like Catlab's `SchGraph`):
`katzen.schema.knowledge` is a generic knowledge graph; `katzen.schema.clojure-code`
a minimal code graph. An application adds its fields with two functors and lifts
its live store in one call:

- **`merge-schema base ext`** — extend a base with your domain morphisms.
- **`rename-schema schema idents`** — bind abstract names to a store's idents.
- **lifting** — wrap a *live, app-managed* datahike connection as an ACSet (e.g.
  dvergr's `kb.schema/as-acset`: install only the missing idents, leave the app's
  columns alone). The app's store stays the source of truth; the ACSet is a view.

So the usual flow is `(rename-schema (merge-schema base domain-ext) idents)` for
the schema, and `as-acset` to view a running store. Two stores that agree on an
`Identity` attr (a shared URI) cross-reference by one `xref` pullback — no bespoke
join code. See [schemata.md](schemata.md) for worked domains (knowledge, code,
accounting) and why the categorical framing pays off.

This is the key architectural choice: **integration is by *optional lifting*, not
by rewriting the domain on katzen.** A mature app keeps its implementation and
gains the categorical operations (xref, equation-checking, rendering, migration)
through a thin adapter.

## See also

- [schemata.md](schemata.md) — schemas/ACSets, equations, type-side, aggregation,
  migration, xref (the data side).
- [composition.md](composition.md) — operads, `oapply`, dynamical systems,
  directed machines / control (the dynamics side).
- [CONVENTIONS.md](CONVENTIONS.md) — `Sch*`/`Th*` split, the accumulate-not-zero
  contract, the optional-alias table.
