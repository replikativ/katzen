# katzen for Clojurians

A bridge from familiar Clojure concepts to the categorical vocabulary
this library uses. If you've used `clojure.spec`, datalog, transducers,
or core.async, you already have most of the intuition — just under
different names.

## The TL;DR

| Categorical name | Clojure analogue | Where it shows up here |
|---|---|---|
| GAT (generalized algebraic theory) | A `clojure.spec`-like signature + axioms | `katzen.theory/deftheory` |
| Theory morphism | A protocol-translation function (`Foo → Bar`) | `katzen.morphism/defmorphism` |
| ACSet (attributed C-set) | A typed datalog database with a static schema | `katzen.acset/vector-acset` |
| Schema | A datalog schema; a `clojure.spec` for relational shape | `:objects`, `:homs`, `:attrs`, `:axioms` in a map |
| Schema morphism | A "view" or schema rewrite | `katzen.acset.migration/schema-morphism` |
| Δ-migration | A datalog view that pulls columns through a schema morphism | `katzen.acset.migration/migrate` |
| ACSet morphism | A morphism of records that respects all relations | `katzen.acset.morphism/acset-morphism` |
| Naturality | "The diagram commutes" — `f(g(x)) = g'(f'(x))` at every record | `katzen.acset.morphism/natural?` |
| Homomorphism search | A pattern-matching CSP — find every embedding of probe into target | `katzen.acset.homomorphism/homomorphisms` |
| FinSet | A finite set `{0, 1, …, n-1}` | `katzen.finset/fin-set` |
| FinFunction | A vector of integer images | `katzen.finset/fin-function` |
| Limit (product, pullback) | "All ways of combining" with consistency conditions | `katzen.finset.limits` |
| Colimit (coproduct, pushout) | "All ways of gluing" with quotient relations | `katzen.finset.colimits` |
| UWD (undirected wiring diagram) | A topology of channels with named junctions | `katzen.uwd` |
| DWD (directed wiring diagram) | A topology of channels with explicit input/output ports + wires | `katzen.dwd` |
| `oapply` | Substitute sub-systems into a wiring pattern; like operadic composition | `katzen.uwd.dynamics/oapply`, `katzen.dwd.dynamics/oapply-dwd` |
| Operad algebra | A protocol implementation that interprets every wiring pattern | `katzen.uwd.algebras/Algebra`, `RasterCompilable` |
| Petri net | A bipartite graph of species and reactions; a typed dataflow graph | `katzen.petri/petri` |
| Reaction network | Petri net + per-reaction rate laws (mass-action, Hill, …) | `katzen.reaction` |

## What is a GAT, really?

If you've written a `clojure.spec` like

```clojure
(s/def ::natural (s/and integer? (complement neg?)))
(s/def ::list-of-naturals (s/coll-of ::natural))
```

you've sketched a *type theory in miniature*: types (`::natural`,
`::list-of-naturals`), constructors (the `coll-of` combinator), and
implicit invariants (non-negativity).

A GAT generalizes this idea:

- **Types** can depend on other types' values. E.g. `Hom(a, b)` is a
  type that depends on two values `a` and `b` (the source and target
  of a morphism). `clojure.spec` doesn't do dependent types directly.
- **Term constructors** are typed operations: `(compose f g)` takes two
  morphisms with matching endpoints and produces a third. The type
  system makes endpoint-matching a compile-time check.
- **Axioms** are equational laws: `compose(compose(f,g), h) =
  compose(f, compose(g,h))`. These don't add new types — they declare
  that two terms denote the same value.

In Clojure terms: a GAT is a protocol + dependent types + equational
laws. Models of a GAT are *instances* — a model of `ThCategory` could
be FinSet (objects = finite sets, morphisms = functions) or Mat (objects
= naturals, morphisms = matrices).

```clojure
(require '[katzen.theory :as t])

(t/deftheory ThCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))
  (term id
    :ctx [a Ob]
    :ret (Hom a a))
  (axiom assoc
    :ctx [a Ob, b Ob, c Ob, d Ob,
          f (Hom a b), g (Hom b c), h (Hom c d)]
    (= (compose (compose f g) h)
       (compose f (compose g h)))))
```

`katzen.stdlib.core` ships `ThCategory`, `ThMonoid`, `ThGroup`,
`ThSchema`, `ThSymmetricMonoidalCategory`, plus the worked algebra
hierarchy in `katzen.library.algebra.*`.

## What is an ACSet?

A C-Set is a functor from a small category C into Set; an attributed
C-Set (ACSet) additionally allows attribute-typed columns alongside
ordinary entity relations. Concretely: it's a typed relational store
backed by a schema.

If you've used datalog (datomic, datalevin, datahike), this will feel
familiar:

```clojure
(require '[katzen.acset :as a])

;; Schema: a directed graph has Vertices (V), Edges (E), and two homs
;; src, tgt : E → V.
(def SchGraph a/SchGraph)

;; An instance: 3 vertices, 3 edges forming a triangle.
(let [[g _]  (a/add-parts g :V 3)
      [g _]  (a/add-edge g 1 2)
      [g _]  (a/add-edge g 2 3)
      [g _]  (a/add-edge g 3 1)]
  g)
```

ACSets serve two roles a Clojurian would normally split across two
tools:

1. **A datalog-style relational store** with a schema. (datahike-backed
   ACSets via the `:datahike` alias make this explicit — every ACSet
   relation becomes a datahike attribute, every part-id becomes an
   entity id.)
2. **A typed data structure for category-theoretic algorithms**. The
   homomorphism backtracker, schema migration, and oapply
   composition all operate on the same ACSet protocol.

The two backends — vector (in-memory persistent) and datahike
(persistent durable) — share the same `IACSet` protocol and the same
`RasterCompilable` compile pipeline. Petri net dynamics work identically
on both.

## What is `oapply`?

If you've written a transducer pipeline like

```clojure
(eduction (comp (map f) (filter g) (take 5)) input)
```

you've already used operadic composition — `comp` substitutes
transducers into a fixed shape (a linear sequence). `oapply` is the
same idea, generalized:

- The "shape" is a wiring diagram (UWD or DWD) instead of a linear
  composition.
- The "operands" are systems with declared interfaces — for a Petri
  net, the interface is the species exposed at ports.
- The "result type" is parameterized by an *algebra* — for a UWD,
  composing relations gives a bigger relation; composing Petri nets
  gives a bigger Petri net; composing ODE systems gives a coupled ODE
  system.

```clojure
(require '[katzen.uwd :as uwd])
(require '[katzen.uwd.dynamics :as ud])

(def sir-uwd                ; 3 junctions: S, I, R
  (let [d (uwd/uwd)
        [d js]   (uwd/add-junctions d 3)
        [d B1 _] (uwd/add-box-with-ports d (take 2 js))  ; infection (S, I)
        [d B2 _] (uwd/add-box-with-ports d (drop 1 js))] ; recovery  (I, R)
    d))

(def composite
  (ud/oapply sir-uwd
             {1 (ud/from-compilable infection-dynamics [1 2])
              2 (ud/from-compilable recovery-dynamics  [1 2])}))
```

The composite shares the `I` species across the two boxes (because both
boxes' second-port wires to the same junction). The resulting state
layout has 3 slots (S, I, R) even though the boxes individually had 2
species each.

## Why FinSet, and what are limits?

FinSet — the category of finite sets and functions — is the
"computational backbone" of much applied category theory because
- objects are just integers (`{0, 1, …, n-1}`),
- morphisms are vectors of integer images,
- limits and colimits have efficient explicit formulas.

A **product** of FinSets is the Cartesian product (size m × n). A
**coproduct** is the disjoint union (size m + n). A **pullback** of a
cospan `A → C ← B` enumerates `{(a, b) | f(a) = g(b)}`. A **pushout**
of a span identifies elements pairwise via union-find.

If this sounds abstract: pullback = "inner join on f(a) = g(b)";
pushout = "union with equivalence-class collapse". Both are
unsurprising operations dressed up in categorical language.

```clojure
(require '[katzen.finset :as fs])
(require '[katzen.finset.colimits :as colim])

;; Pushout of {0,1} -f-> {0,1,2} and {0,1} -g-> {0,1} with f=[0 1], g=[0 0]
;; (g identifies both source elements with target 0).
(def po (colim/pushout
         (fs/fin-function [0 1] 3)
         (fs/fin-function [0 0] 2)))
(fs/cardinality (:apex po))   ;; => 3 — classes {0_A, 1_A, 0_B}, {2_A}, {1_B}
```

The composition algorithms for UWDs (coequalizer in FinSet) and DWDs
(coproduct in FinSet) both reduce to FinSet (co)limits. Catlab uses
exactly the same algorithms; we ship them in
`katzen.finset.{limits, colimits}`.

## What does the verification path buy you?

Through `katzen.acset.theory-bridge` (opt-in via the `:ansatz` alias),
a schema with declared axioms can be **proved** to preserve those
axioms under a schema morphism — the bridge translates the schema and
its morphisms into ansatz's CIC encoding (the same kernel Lean 4 uses)
and discharges every dom-axiom obligation in the codomain.

```clojure
(tb/verify-schema-morphism! my-morphism)
;; → :ok           (every dom axiom discharged in the codom; kernel-checked)
```

Catlab and GATlab parse the axioms but explicitly skip this step.
GATlab's source comment reads: *"axioms are not mapped to proofs.
TODO."* We close that TODO via the bridge.

## Where to look next

- [`tutorial.md`](tutorial.md) — a 15-minute walkthrough that puts these
  pieces together.
- [`CONVENTIONS.md`](CONVENTIONS.md) — the project's internal
  conventions (the `Sch*` vs `Th*` split, the `RasterCompilable`
  contract, the alias structure).
- `dev/notebooks/comparison_with_catlab.clj` — a runnable side-by-side
  with the Julia stack, sectioned by capability, with file:line cites
  to the equivalent Catlab/GATlab code.
- The [Catlab.jl documentation](https://algebraicjulia.github.io/Catlab.jl/dev/)
  remains the canonical reference for the category-theoretic concepts.
  Most of the underlying mathematics is well-explained there; the
  vocabulary is identical in our port.
