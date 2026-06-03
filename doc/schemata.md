# Schemata: categorical schemas, and why they're worth it

katzen lets you describe a domain as a **schema** and your data as an **ACSet**
over it. This guide explains the model, the one rule for where things live
(machinery here, domains in your app), three worked domains — **knowledge**,
**code**, **accounting** — and why doing it categorically pays off over a pile
of datahike attributes and ad-hoc joins.

## 1. The model in one breath

An **ACSet** on a schema `C` is a functor `C → Set`. Concretely a schema is a
plain map:

```clojure
{:objects   [Sym …]                              ; the entity sorts
 :homs      [{:name Sym :dom Sym :codom Sym
              :cardinality :one|:many}]           ; foreign keys (functions / relations)
 :attr-types [Sym …]                              ; value sorts (String, Long, Identity, …)
 :attrs     [{:name Sym :dom Sym :codom Sym
              :unique :db.unique/… }]              ; typed columns
 :equations [{:dom Sym :lhs [m…] :rhs [m…]}]}      ; path equations = invariants
```

and the data is the parts of each object plus, for each morphism, a function
from its domain parts to its codomain (a part-id for a Hom, a value for an
Attr). That's it. Everything below is an operation *on* this.

- **Identity** — a distinguished attr-type for cross-ACSet references: a URI
  string both schemas agree on. An *internal* reference is a Hom (an eid, valid
  only in one store); a reference that must cross a store boundary is an Attr
  valued in `Identity`. Resolving it is a **pullback** over `Identity`
  (`katzen.xref`).
- **Cardinality** — `:one` is a function (an ACSet morphism proper); `:many` is
  a relation, stored as a native datahike cardinality-many column. A relation
  that carries *its own* data is **reified** — i.e. it's just another Object
  (a junction), not a special case.
- **Equations** — two morphism paths out of an object that must agree on every
  part. The schema's *invariants*, checked by `katzen.acset.check/check-axioms!`
  (`:equations` desugar into `:axioms`).
- **Type-side** — attr-types are sorts of a GAT; operations on them (formulas,
  predicates) are terms, evaluated by `katzen.eval`. Computed properties are
  terms-in-context; validation is a Bool-valued term.
- **Aggregation** — a rollup is a fold with a commutative **monoid** (sum /
  count / min / max / …): `katzen.aggregate`. The spec; for scale it lowers to
  an OLAP engine (stratum) or datalog.
- **Migration** — a schema morphism induces functorial data migration (Δ/Σ/Π):
  schema evolution and views that are *structure-preserving by construction*.

## 2. The one rule: machinery here, domains in your app

> **katzen ships the machinery and the generic vocabulary. A domain schema is a
> *presentation that uses* katzen, and lives in the app that owns the domain.**

`katzen.schema.*` carries only *general* shapes (the way Catlab ships
`SchGraph`) — e.g. `katzen.schema.knowledge` is a generic knowledge *graph*
(`Entity` with `title`/`summary`/`kind`/`links`), nothing more. Your
application's fields are added with two functors:

- **`merge-schema base ext`** — extend a base with your domain morphisms (a
  schema inclusion). dvergr adds `employer`/`role`/`mention-count`/`url` to the
  generic KB schema this way; they do **not** belong in katzen.
- **`rename-schema schema idents`** — bind abstract names to a store's concrete
  idents (`:title → :entity/title`). A pure renaming is an iso.

So the usual bind-an-extended-schema flow is
`(rename-schema (merge-schema base domain-ext) idents)`.

## 3. Three domains, the same constructs

| | **knowledge** (dvergr) | **code** (dvergr) | **accounting** (kontor) |
|---|---|---|---|
| objects | `Entity` | `Def` | `Account`, `Posting`, `Txn` |
| homs | `links` (many) | — | `account.parent`, `distribution` (junction) |
| Identity attr | `title` (wiki URI) | `qname` | account code / entity URI |
| typed attrs | summary, kind | file, source | amount, commodity, date |
| many-relation | `links` | `refs` (many) | postings of a txn |
| **invariant** (equation) | — | — | **Σ debits = Σ credits** per `(entity×ledger×commodity)` |
| **aggregation** (monoid) | mention-count | — | **balance / trial balance / P&L** |
| evolution (functor) | — | — | per-country localization, tax-as-data |

The point of the table: a knowledge graph, a code dependency graph, and a
double-entry ledger are *the same kind of object* — an ACSet — and their
constraints, rollups, references and evolution are *the same operations*.

**Accounting makes the value concrete.** Double-entry's balance rule is an
`:equation`/axiom checked by the same engine as any structural law. An account
**balance** is a monoid (group) fold over its postings — the same `aggregate`
as a knowledge mention-count. A "what-if" period is a yggdrasil **fork** — the
same copy-on-write substrate as a code proposal; bitemporal is its native read.
A customer in the ledger ⟷ a knowledge entity ⟷ a page resolve by the shared
**Identity** URI — one `xref` pullback, not bespoke join code. Adding a
jurisdiction is *extending the presentation* — functorial migration, provable,
not subclassing.

## 4. Why bother (over flat datahike + joins)

- **One vocabulary across domains.** Objects/homs/attrs, Identity, equations,
  monoids, functors — learn it once; it covers graphs, ledgers, code.
- **Invariants are first-class and checkable**, not scattered assertions:
  sum-to-zero, "no dangling reference", "src ≠ tgt" are equations the kernel
  enforces.
- **Aggregations are defined, not re-coded** per report — balances, rollups,
  mention-counts are all the same monoid fold (and lower to a real OLAP engine).
- **Cross-domain links for free** — a shared `Identity` + the pullback join the
  KB, the code index, and the ledger without per-pair glue.
- **Evolution is structure-preserving** — schema morphisms (Δ/Σ/Π) migrate data
  and views with guarantees; the schema is a value you can compose
  (`merge-schema`), rename, and *verify*, not ad-hoc datoms.

The cost is one idea — *the schema is a small category, the data is a functor*.
Everything you'd otherwise hand-roll (constraints, aggregates, joins, migrations)
becomes a named categorical construction you get to reuse.
