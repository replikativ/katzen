(ns katzen.schema.knowledge
  "The KNOWLEDGE-BASE schema as a canonical, backend-agnostic katzen ACSet
   schema (part of katzen's standard-schema library, `katzen.schema.*`). It is
   the single formal definition shared by dvergr and simmis; each binds it to
   its own store idents via `katzen.acset/rename-schema`.

   Categorical shape:
     - Object `Entity` — a knowledge entity (the [[wiki-link]] node).
     - Hom `employer` — the cardinality-ONE person→company edge (Entity→Entity).
     - Hom `links` — the cardinality-MANY entity↔entity links, as a NATIVE
       datahike cardinality-many ref (not a junction object). A plain
       many-relation maps onto what datahike already does; a junction object is
       reserved for a REIFIED relation (one carrying its own attributes — e.g. a
       Membership with a `since` date), which is just another Object.
     - Attr `title` : Entity → Identity (unique) — the entity name AS A URI, the
       shared cross-ACSet join key (see `katzen.xref`).
     - The remaining attrs are the typed property columns.

   Names are ABSTRACT (no store idents). dvergr binds them to its `:entity/*`
   datahike idents; simmis installs them into its category-S as Object/Morphism
   blocks (via the meta-bridge).")

(def schema
  "Canonical knowledge-base ACSet schema (abstract names)."
  {:name :Knowledge
   :objects   [:Entity]
   :homs      [{:name :employer :dom :Entity :codom :Entity}
               {:name :links    :dom :Entity :codom :Entity :cardinality :many}]
   :attr-types [:Identity :String :Keyword :Long :Instant]
   :attrs     [{:name :title         :dom :Entity :codom :Identity :unique :db.unique/value}
               {:name :summary       :dom :Entity :codom :String}
               {:name :kind          :dom :Entity :codom :Keyword}
               {:name :url           :dom :Entity :codom :String}
               {:name :role          :dom :Entity :codom :String}
               {:name :mention-count :dom :Entity :codom :Long}
               {:name :created-at    :dom :Entity :codom :Instant}
               {:name :updated-at    :dom :Entity :codom :Instant}]
   :equations []})

(def identity-attr
  "The Attr carrying an Entity's shared cross-ACSet Identity (its URI)."
  :title)
