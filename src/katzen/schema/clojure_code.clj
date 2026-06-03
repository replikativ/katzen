(ns katzen.schema.clojure-code
  "The CLOJURE-CODE schema as a canonical, backend-agnostic katzen ACSet schema
   (part of `katzen.schema.*`). A projection of source text — Defs keyed by a
   qname Identity, plus a Ref junction for the references each def makes — shared
   by dvergr (the in-memory code index) and simmis (rendering/cross-reference).

   Categorical shape:
     - Object `Def` — a top-level definition.
     - Attr `qname` : Def → Identity — the def's URI (the shared join key).
     - Attr `refs` : Def → Identity (cardinality MANY) — the qname URIs this def
       references, as a NATIVE datahike cardinality-many column (not a junction
       object — the references carry no data of their own). Resolving which refs
       land on a project Def is a pullback over Identity (`katzen.xref`);
       find-references is `incident :refs`.

   Names are ABSTRACT. dvergr binds them to its `:def/*` idents.")

(def schema
  "Canonical Clojure-code ACSet schema (abstract names)."
  {:name :ClojureCode
   :objects   [:Def]
   :homs      []
   :attr-types [:Identity :String]
   :attrs     [{:name :qname  :dom :Def :codom :Identity}
               {:name :file   :dom :Def :codom :String}
               {:name :source :dom :Def :codom :String}
               {:name :refs   :dom :Def :codom :Identity :cardinality :many}]
   :equations []})

(def identity-attr
  "The Attr carrying a Def's shared cross-ACSet Identity (its qname URI)."
  :qname)
