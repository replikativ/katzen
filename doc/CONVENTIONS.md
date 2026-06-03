# katzen conventions

Short reference for the architectural choices a returning reader needs.

## Schemas vs theories: the `Sch*` / `Th*` split

Two namespaces of names describe categorical structure:

- **`Sch*`** — *schemas*, plain Clojure maps of shape
  ```clojure
  {:name 'SchGraph
   :objects [:V :E]
   :homs       [{:name :src :dom :E :codom :V} …]
   :attr-types []
   :attrs      []
   :axioms     []}      ; optional
  ```
  Schemas live in `katzen.acset.*` (e.g. `katzen.acset/SchGraph`,
  `katzen.acset.graphs/SchSymmetricGraph`). They are the contract an
  ACSet (an instance of `katzen.acset/IACSet`) is built against.

- **`Th*`** — *theories*, runtime GAT values built with
  `katzen.theory/deftheory`. They live in `katzen.stdlib.core` (e.g.
  `ThGraph`, `ThCategory`, `ThMonoid`). Theories are full GAT
  presentations — type/term constructors plus axioms — that the
  ansatz Lean kernel can admit.

`katzen.acset.theory-bridge` translates between them: `schema->theory`
turns any schema map (including its `:axioms`) into a GAT, and
`schema-morphism->theory-morphism` turns a `SchemaMorphism` into a
`TheoryMorphism` ready for `katzen.ansatz.export/check-morphism!`.
Don't try to use a schema where a theory is expected — go through the
bridge.

The split is *intentional*: schemas are lightweight data the ACSet
machinery needs; theories carry the additional structure ansatz wants.
Most code paths only deal with schemas; verification is the only path
that bridges into theories.

## The accumulate-not-zero contract for `RasterCompilable`

`katzen.compile.core/RasterCompilable` has three methods:

- `(-state-layout x)` — return a `StateLayout` describing the state
  vector (`:size` and `{label → slot}`).
- `(-raster-body x layout)` — return a sequence of source forms.
- `(-clojure-body x layout)` — return a `(fn [du u t])`.

**Bodies MUST accumulate, NOT zero `du`.** Each form / fn writes
`du[i] += contribution`, never `du[i] = contribution`. The
`compile-rhs` driver emits a single `aset du i 0.0` block at the top
of the ftm before splicing in the body. This contract is what lets
composite forms concatenate per-box bodies cleanly — when two boxes
sharing a junction both write to the same global slot, summing into
`du` does the right thing automatically.

If you implement `RasterCompilable` and your body emits a zero-out,
you'll double-zero in composition and silently produce wrong
dynamics. The 3.3.g–h work and every CRS/Machine/CPG composite assume
this invariant.

## Optional aliases and what they unlock

The default `:test` run is clean: no GPU, no kernel, no embedded
database. Heavyweight dependencies live behind opt-in aliases:

| Alias | Adds | Unlocks |
|---|---|---|
| `:datahike` | `io.replikativ/datahike` (local path), `persistent-sorted-set` | `katzen.acset.datahike/DatahikeACSet`; persistent ACSet backend with datalog queries |
| `:raster` | `org.replikativ/raster` plus `--add-modules=jdk.incubator.vector` JVM flag | `katzen.compile.core/compile-rhs` returns a typed `raster.fn.IFn__doubles_doubles_double` that `raster.ode/solve` accepts |
| `:ansatz` | `org.replikativ/ansatz` (local path) plus `--enable-native-access=ALL-UNNAMED` | `katzen.ansatz.export/check-morphism!`; the bridge-based `verify-schema-morphism!` / `verified-migrate`. Requires a pre-built Mathlib store at `/var/tmp/ansatz-mathlib` (see `../ansatz/scripts/setup-mathlib.sh`) |
| `:dev` | `tools.namespace`, paths for `dev/` and `examples/` | Loads `dev/notebooks/migration_demo.clj` etc. |

Test aliases mirror these: `:test-raster` (with `test-raster/` on the
path), `:test-ansatz`, `:test-datahike`. The base `:test` covers
everything that doesn't require an alias.

The `katzen.test-support` namespace in `test/` centralizes the
availability checks (`datahike-available?`, `raster-available?`,
`ansatz-ready?`) and the standard `skip-notice` helper.

## Backend transparency for dynamics

`PetriDynamics` and `ReactionDynamics` work on both `VectorACSet` and
`DatahikeACSet` without any per-backend code. The protocol's
`StateLayout` uses whatever the ACSet returns as part-ids — 1-based
contiguous ints on vector, datahike eids on datahike — and maps them
to 0..n-1 slots through the layout's `:index-of`. The compiled raster
body is identical either way.

If you write a new categorical concept implementing `RasterCompilable`,
the same property comes for free as long as you build your layout via
`(cc/state-layout (parts-seq))` rather than hard-coding indices.
