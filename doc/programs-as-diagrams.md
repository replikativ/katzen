# Programs and dynamics as string diagrams

This note locks the design for mapping **functional code** (and, in the same
breath, **dynamical/control systems**) into **string/wiring diagrams** — so we
expose the core structure of the code without building an incoherent hybrid. It
grounds the choice in Dusko Pavlović's *Programs as Diagrams* (Springer 2023) and
in what Catlab / AlgebraicDynamics already do, and it fixes one discipline:

> **Every diagram primitive must be a named categorical construction. The diagram
> we render is a *view* (a functor on the diagram); the morphism it denotes must
> be a coherent term in a real monoidal category.**

## 1. The thesis

A pure function and a control loop are *the same kind of object*: a morphism in a
monoidal category, drawable as a wiring diagram, composable by `oapply`. katzen
already carries the target — UWD/DWD/CPG wiring-diagram ACSets and the unified
`katzen.compose/oapply`. So:

| | source | lands in | composed by |
|---|---|---|---|
| pure Clojure fn | a `defn` body | a **cartesian** string diagram | `oapply` |
| control system | sensor+controller+plant | a **directed** (DWD) diagram | `oapply` |
| resource sharing / shared state | atoms, channels | an **undirected** (UWD / hypergraph) diagram | `oapply` |

One diagrammatic substrate; the *category* differs by what fills the boxes. The
**cartesian bead** (§3) unifies it: purity is the structural property that, on the
code side licenses optimization (CSE/dead-code), and on the dynamics side marks a
clean, history-free component.

## 2. Grounding: the monoidal computer (Pavlović)

We model code as a **symmetric monoidal copy/discard (CD-) category** with a
program object `P` and a universal evaluator `run : P × A → B` — a *monoidal
computer* — **not** a cartesian *closed* category. Why this and not the classic
λ/CCC route:

- `run` is a **surjection** (many programs → one function), not currying's
  bijection. Program identity matters (intensionality); it's what makes
  `eval`, self-reference, and partiality expressible. Clojure *has* `eval`,
  macros, and first-class fns — the monoidal computer is the model built for
  exactly such languages.
- Computation is **partial**: applying the fixpoint trick to `not` forces a
  divergent, non-cartesian value, so a faithful model *cannot* stay cartesian.
  The cartesian part is a *subcategory* — the pure, total fragment.

Reference: [[ref-pavlovic-programs-as-diagrams]] (Ch.1 wiring/copy-delete/bead;
Ch.2 run/types-as-idempotents; Ch.3 fixpoints; Ch.8 program-closure).

## 3. The construct → diagram map (Dusko's approach)

The functor `⟦·⟧` takes a function **body** (its sub-form structure, from the
tools.analyzer AST / sexpr — NOT the L2 interface ACSet, which is the *between*-
functions call graph) to a wiring diagram.

| Clojure construct | diagram part | categorical structure |
|---|---|---|
| `(defn f [a b] …)` | outer box; params → input **ports** (types = annotation or `Any`) | SMC |
| value/literal | a 0-input box (`I → X`) | monoidal unit |
| `(g x y)` | a **box** `g`; wires from `x`,`y`'s ports in | `∘` |
| `(g (f x))` | composition | `∘` |
| `(let [x e …] body)` | wiring; the *name* labels the resulting output port | SMC |
| binding used N× | **copy** `Δ` (fan-out, explicit) | comonoid (CD) |
| binding used 0× | **delete** `▪` (ground, explicit) | comonoid (CD) |
| **pure, total fn** | the **cartesian bead `•`** — licenses CSE/DCE | cartesian sub-cat |
| `(if c t e)` | a **`cond` box**: a control port + the two branches as **nested sub-diagrams** (= the branch program-codes) | **lazy branching** `ift(c, ⌜t⌝, ⌜e⌝) = {{c}(⌜t⌝, ⌜e⌝)}` — select a program code via `{c}`, then `run` it; only one branch runs (Pavlović §3.6.1, Eq 3.31). A well-defined `if` wants a **decidable** (pure/total = beaded) predicate (§2.4.1). |
| `(fn …)` literal | a **`:program` box** ⌜·⌝ (an element of `P`) with its body as a nested sub-diagram; free vars wire in as the closure | monoidal computer |
| applying a fn value: `(g x)` (g local), `((fn …) x)`, HOF | a **`:run`/apply box** — Dusko's `{g} x` | monoidal computer |
| bare fn ref as a value (`inc` in `(map inc xs)`) | a **`:program` code box** ⌜inc⌝ | monoidal computer |
| `loop`/`recur`, self-recursion | a **trace feedback loop** (rendering) | semantics: Kleene **fixpoint** (`run`+copy) |
| atoms / channels / shared state | a **junction** (shared resource) | hypergraph / UWD |

Settled choices (Dusko's approach for now; the CCC/coproduct/traced alternatives
are understood as the classical counterparts, to revisit):

- **Higher-order = `run`/`P`**, not an internal hom. Honest for a language with
  `eval`/macros.
- **Recursion = a fixpoint**, *rendered* as a trace loop (see §4).
- **Conditionals = Pavlović's lazy branching** (§3.6.1, Eq 3.31):
  `ift(c, F, G) = {{c}(F, G)}` — the branches are **program codes** `F = ⌜then⌝`,
  `G = ⌜else⌝`; `{c}` selects one code; the outer `run` evaluates it; **only one
  branch runs**. We render the two codes as nested sub-diagrams and the selection
  as a `cond` box (a *view* of this morphism — see §4). This is purely the
  monoidal computer: no operad, no coproduct, no CCC. (It is the categorical
  reason Catlab's straight-line `@program` can't do `if`: branching is not a
  monoidal primitive — it needs `run` (Pavlović), or coproducts/distributivity
  (the classical route Pavlović names in §1.7.3 and deliberately sets aside). The
  branches *being programs* is the whole "programs are data" thesis. NB: the
  earlier "operadic" framing was wrong — `oapply` is *static* composition of
  whole diagrams, not runtime branch selection.)

## 4. Semantics vs rendering — the anti-frankenstein rule

The morphism a diagram denotes is a coherent CD-category term. **What we draw is a
separate functor** from that morphism to a layout. This split is what lets us be
*legible* without being *incoherent*:

- recursion: semantics = a Kleene fixpoint; rendering = a labelled **trace loop**.
- `if`: semantics = `ifte`/`run` on a Boolean; rendering = a **two-compartment
  box** holding the branch sub-diagrams.

The diagram itself is a **wiring-diagram ACSet**, so rendering to mermaid,
graphviz, or a ReactFlow/n8n-style interactive canvas is the *same* "render an
ACSet" functor (the P4 render functor) with different back ends. Ports = handles,
wires = edges. Start with **mermaid** (instant, embeddable); **ReactFlow** is the
right interactive target for simmis (typed handles ≈ ports). No back-end choice is
load-bearing — they're all serializations of one ACSet.

## 5. What this needs from katzen

- **Reuse:** the wiring-diagram ACSets (`katzen.dwd`/`katzen.uwd`) and `oapply`
  are the target; `cond`/`copy`/`delete`/`run` boxes are box *values* in the DWD.
- **Add (the monoidal-operations layer Catlab has, we partially lack):** wiring
  diagrams as morphisms in a *free* symmetric-monoidal CD-category — the
  operations `compose`/`otimes`/`mcopy`/`delete`/`trace` and the theory hierarchy
  `SMC → cartesian → traced → hypergraph` with diagram models (Catlab's
  `MonoidalDirected.jl`). katzen has `ThSymmetricMonoidalCategory` as a GAT but
  not the wiring-diagram models of these operations. This is additive rigor — the
  principled foundation — and a real katzen improvement independent of the code
  functor. It is *not* required to ship the first rendered demo (which can build
  the DWD ACSet directly), but it is where copy/delete/trace become first-class.

## 6. Phasing

1. **doc (this).**
2. **Cartesian straight-line functor** `⟦defn-body⟧ → wiring-diagram ACSet` —
   params/let/calls/copy/delete + the purity bead — and **render to mermaid**.
   Demo: a real pure Clojure fn shown as a diagram with shared/dropped wires and
   beads, noting the *same* diagram type carries the SIR/control composites.
3. **`cond` box** (lazy branching, nested branches) — *done*: `if`/`when`/`cond`
   become a `:cond` box (a hexagon) whose two branches are walked into grouped
   sub-diagrams (the branch program-codes) and rendered as nested subgraphs; the
   condition + both branch outputs wire into the box (the selection); shared
   in-scope values flow into both branches. Semantics = Pavlović's lazy
   `ift(c, ⌜then⌝, ⌜else⌝) = {{c}(⌜then⌝, ⌜else⌝)}` (§3.6.1, Eq 3.31) — select a
   code, then `run` it; only one branch runs. Pure monoidal computer — no operad,
   no coproduct. (The nested-branch *drawing* is the view; that both are drawn
   exposes structure, that one runs is the semantics.)
4. **HOF** via `run`/`P` box — *done*: `fn` literals are `:program` boxes ⌜·⌝ (body nested); applying a fn value is a `:run`/apply box (`{g} x`); a bare fn ref is a `:program` code box. Pure monoidal computer (programs are data).
5. **Recursion** as a trace-rendered fixpoint.
6. **Monoidal-operations layer** in katzen (SMC/cartesian/traced theories +
   wiring-diagram `copy`/`delete`/`trace`); **ReactFlow** renderer in simmis.

The flexibility is real but *located*: it is the freedom to map each code
fragment to the category that honestly models it (cartesian / CD+run / traced /
hypergraph) — all real, well-studied categories that nest coherently — and to
choose a legible rendering of the resulting morphism. It is **not** freedom to
invent primitives with no morphism behind them.

## See also
- [composition.md](composition.md) — `oapply`, dynamical systems, directed
  machines / control (the dynamics half of the same diagrammatic world).
- [architecture.md](architecture.md) — the core + backends.
- Pavlović, *Programs as Diagrams* (Springer 2023); Catlab `ParseJuliaPrograms`
  (`@program`); Selinger, *A survey of graphical languages for monoidal categories*.
