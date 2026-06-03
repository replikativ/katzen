(ns katzen.schema.knowledge
  "A GENERIC knowledge-base schema as a canonical, backend-agnostic katzen ACSet
   schema (part of katzen's standard-schema library, `katzen.schema.*` — the way
   Catlab ships `SchGraph`). It holds only the universal shape of a knowledge
   graph; it carries NO application-domain fields. Consumers EXTEND it with their
   own properties via `katzen.acset/merge-schema` and bind to store idents via
   `katzen.acset/rename-schema` (see doc/schemata.md).

   Categorical shape:
     - Object `Entity` — a knowledge node (the [[wiki-link]] page).
     - Hom `links` — the cardinality-MANY entity↔entity graph, as a NATIVE
       datahike cardinality-many ref (not a junction object). A plain
       many-relation maps onto what datahike already does; a junction object is
       reserved for a REIFIED relation (one carrying its own attributes — e.g. a
       Membership with a `since` date), which is just another Object.
     - Attr `title` : Entity → Identity (unique) — the node name AS A URI, the
       shared cross-ACSet join key (see `katzen.xref`).
     - `summary`, `kind`, `created-at`, `updated-at` — universal node fields.

   Domain extensions live with their app, NOT here: e.g. dvergr's CRM-flavoured
   `employer` / `role` / `mention-count` / `url` are added in dvergr's binding,
   not baked into this general schema.")

(def schema
  "Generic knowledge-graph ACSet schema (abstract names)."
  {:name :Knowledge
   :objects   [:Entity]
   :homs      [{:name :links :dom :Entity :codom :Entity :cardinality :many}]
   :attr-types [:Identity :String :Keyword :Instant]
   :attrs     [{:name :title      :dom :Entity :codom :Identity :unique :db.unique/value}
               {:name :summary    :dom :Entity :codom :String}
               {:name :kind       :dom :Entity :codom :Keyword}
               {:name :created-at :dom :Entity :codom :Instant}
               {:name :updated-at :dom :Entity :codom :Instant}]
   :equations []})

(def identity-attr
  "The Attr carrying an Entity's shared cross-ACSet Identity (its URI)."
  :title)
