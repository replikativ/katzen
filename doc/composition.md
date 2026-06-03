# Composing dynamical systems (and why it's tricky)

Building a big dynamical system by wiring up small ones is deceptively hard. This
guide explains the failure modes, the operadic fix katzen uses, the two
composition styles it ships, and how *translation* between systems is itself
functorial. It mirrors the AlgebraicJulia stack (Catlab / AlgebraicDynamics /
AlgebraicPetri), which katzen ports — `katzen.uwd` + `uwd.dynamics` (resource
sharers), `katzen.dwd` + `dwd.dynamics` (machines), `katzen.cpg`, and
`katzen.petri` / `katzen.reaction` (+ `petri.migration`).

## Why naive composition fails

Three concrete ways "just wire the outputs to the inputs / add the ODEs" goes
wrong:

1. **Duplicated shared state.** A predator–prey model is "rabbit growth" +
   "predation" + "fox decline". Glue them naively and you get *two* rabbit
   populations — one that grows, one that gets eaten — and *two* fox
   populations. There is only one of each; the growth term and the predation
   term act on the *same* variable. (SIR is the same: the `I` produced by
   infection is the *same* `I` consumed by recovery.)
2. **Ill-defined feedback.** When an output feeds back into an input, "evaluate
   in order" is ambiguous; you need a principled readout, not a call order.
3. **Non-associativity / non-reuse.** Hand-wired composites can't be treated as
   a black box and re-composed; `(A∘B)∘C` and `A∘(B∘C)` drift apart.

## The fix: operads of wiring diagrams

Split composition into **syntax** and **semantics**.

- A **composition pattern** is a morphism in an *operad of wiring diagrams* —
  pure combinatorics: boxes, ports, junctions/wires, an outer interface. It says
  *how* parts connect, nothing about their behavior.
- **`oapply`** is an *operad algebra* — a **functor** from that syntax to actual
  systems. It assigns each box a system and computes the composite, **identifying
  shared variables** (a colimit/coequalizer in FinSet) so the rabbit-growth and
  predation terms accumulate into the *one* rabbit slot.

Because `oapply` is functorial, composition is **associative and hierarchical**
(compose composites), **correct by construction** (shared variables merged, not
duplicated), and **uniform** (the same operation regardless of what the boxes
are).

### Two styles

- **Undirected (UWD) — resource sharing.** Boxes share *variables* via
  junctions; no direction. `katzen.uwd.dynamics/oapply` over a
  `ContinuousResourceSharer`. The algebra is `Dynam`
  ([Baez–Pollard 2017](https://arxiv.org/abs/1704.02051)). This is the SIR/
  Lotka–Volterra "identify the shared population" case — see the README example.
- **Directed (DWD / CPG) — machines.** Boxes have inputs, outputs, and a
  *readout* function; wires feed outputs to inputs. `katzen.dwd.dynamics/
  oapply-dwd` over a `Machine`. The algebras are `CDS`/`DDS`
  ([Vagner–Spivak–Lerman 2015](https://arxiv.org/abs/1408.1598);
  [Schultz–Spivak–Vasilakopoulou–Wisnesky 2019](https://arxiv.org/abs/1609.08086)).
  The classic example is tanks connected by pipes; readouts make feedback
  well-defined. Overview of all three algebras:
  [Libkind 2020](https://arxiv.org/abs/2007.14442).

## Translation is functorial too

Composition's twin is *translation* — and it's also functors, which is why a
composed model can be carried to another representation while preserving its
structure:

- **Open Petri net → open dynamical system.** A reaction network (an ACSet)
  maps to a vector field; *composing nets then taking dynamics* equals *taking
  dynamics then composing* ([Baez–Pollard](https://arxiv.org/abs/1704.02051)).
  katzen: `petri-dynamics`.
- **Black-boxing.** A composed open system maps to the input/output *relation*
  it imposes at steady state — abstract a subsystem to its external behavior.
- **Continuous → discrete.** `euler_approx` is a functor preserving composition
  (compose-then-discretize = discretize-then-compose).
- **Schema migration.** `Δ/Σ/Π` and `katzen.petri.migration` translate model
  *data* between schemas, structure-preservingly.

## Capstone: structured / stratified epidemic models

The payoff at real scale: you don't hand-write a 100-compartment COVID model —
you **stratify** a small base model (e.g. SIR) by structure (age × region ×
vaccination status) using a *generalized product / pullback* of Petri nets, and
the strata compose provably consistently. This is
[Libkind, Baas, Halter, Patterson, Fairbanks, *An algebraic framework for
structured epidemic modelling*, Phil. Trans. R. Soc. A 2022](https://royalsocietypublishing.org/doi/10.1098/rsta.2021.0309)
([arXiv 2203.16345](https://arxiv.org/pdf/2203.16345),
[code](https://github.com/AlgebraicJulia/Structured-Epidemic-Modeling)).

The recipe: a base ACSet model + a *type system* model; the stratified model is
their **pullback** over the type system. katzen has the pieces (Petri ACSets,
`finset` limits/colimits, `petri.migration`); a runnable katzen stratification
notebook is a worthwhile follow-up (TODO) — it's the single most convincing
"compose, don't hand-write" artifact.

## Where this goes: probabilistic simulators

The same operad composes *stochastic* systems: an open Markov process / a
probabilistic dynamical system is another algebra over the same wiring-diagram
operad (sample/observe/factor in the box, junctions share random variables). So
a composed simulator inherits the same guarantees — shared latent variables are
identified, sub-models are reusable black boxes, and you can translate a
composed probabilistic model to its ODE mean-field or to a sampler by a functor.
This is the path for katzen's probabilistic-simulator work: define the
primitives once, compose them with the machinery already here.

## See also

- [doc/schemata.md](schemata.md) — schemas/ACSets, equations, the type-side,
  aggregation, migration, xref (the data side; this doc is the dynamics side).
- the AlgebraicDynamics.jl Lotka–Volterra "three ways" example — the model this
  port follows.
