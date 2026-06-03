(ns katzen.schema.clojure-code
  "The CLOJURE-CODE schema as a canonical, backend-agnostic katzen ACSet schema
   (part of `katzen.schema.*`). A projection of source text — Defs keyed by a
   qname Identity, plus a Ref junction for the references each def makes — shared
   by dvergr (the in-memory code index) and simmis (rendering/cross-reference).

   Categorical shape:
     - Object `Def` — a top-level definition.
     - Object `Ref` — a junction reifying the cardinality-MANY references a def
       makes: `Def ← Ref → Identity`.
     - Hom `from` : Ref → Def — which def makes the reference.
     - Attr `qname` : Def → Identity — the def's URI (the shared join key).
     - Attr `to` : Ref → Identity — the referenced qname URI (resolving which
       refs land on a project Def is a pullback over Identity — `katzen.xref`).

   Names are ABSTRACT. dvergr binds them to its `:def/*` / `:ref/*` idents.")

(def schema
  "Canonical Clojure-code ACSet schema (abstract names)."
  {:name :ClojureCode
   :objects   [:Def :Ref]
   :homs      [{:name :from :dom :Ref :codom :Def}]
   :attr-types [:Identity :String]
   :attrs     [{:name :qname  :dom :Def :codom :Identity}
               {:name :file   :dom :Def :codom :String}
               {:name :source :dom :Def :codom :String}
               {:name :to     :dom :Ref :codom :Identity}]
   :equations []})

(def identity-attr
  "The Attr carrying a Def's shared cross-ACSet Identity (its qname URI)."
  :qname)
