(ns katzen.acset.datahike
  "Datahike-backed implementation of the IACSet protocol.

   An ACSet on schema C maps to a datahike database as follows:

   - Each part of object Ob becomes an entity with the indexed marker
     attribute `:katzen/ob = :Ob` (so `nparts :V` is `[?e :katzen/ob :V]`).
   - Each Hom morphism m : Ob → Ob' becomes a ref attribute `m`
     (`:db.type/ref`). Setting m on a part is a `[:db/add e m target-eid]`.
   - Each Attr morphism m : Ob → AttrType becomes a typed value
     attribute. (Not implemented in this first cut — SchGraph has no
     attributes.)

   Part-ids are datahike entity-ids directly. They are integers, sorted,
   but NOT contiguous (datahike interleaves across objects). The
   backtracker doesn't care; tests that compare against VectorACSet need
   to compare counts and structure, not specific id values.

   Load via the :datahike alias (lives at `:local/root ../datahike`)."
  (:require [datahike.api :as d]
            [katzen.acset :as a]))

(defn- ob-schema-tx
  "The fixed marker attribute used to tag parts by their object."
  []
  [{:db/ident       :katzen/ob
    :db/valueType   :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/index       true}])

(defn- hom->schema-tx
  "Datahike schema attribute for a Hom morphism. We make it indexed for
   incident lookups."
  [{:keys [name]}]
  {:db/ident       name
   :db/valueType   :db.type/ref
   :db/cardinality :db.cardinality/one
   :db/index       true})

(def ^:private attr-type->value-type
  "Map an ACSet attr-type name to a datahike :db/valueType. Covers the common
   scalar attr-types (Catlab's are Julia types like Symbol/String/Int/Float);
   unknown attr-types fall back to :db.type/string."
  {:String  :db.type/string  :Str    :db.type/string
   :Symbol  :db.type/symbol  :Keyword :db.type/keyword
   :Int     :db.type/long    :Integer :db.type/long   :Long :db.type/long
   :Float   :db.type/double  :Double  :db.type/double :Number :db.type/double
   :Bool    :db.type/boolean :Boolean :db.type/boolean
   :UUID    :db.type/uuid    :Instant :db.type/instant :Date :db.type/instant
   :BigInt  :db.type/bigint  :BigDec  :db.type/bigdec})

(defn- attr->schema-tx
  "Datahike schema attribute for an Attr morphism (a typed value column).
   Unlike a Hom (a ref), the value type comes from the attr's codom (an
   attr-type), so values like strings/ints/uuids are stored directly."
  [{:keys [name codom]}]
  {:db/ident       name
   :db/valueType   (get attr-type->value-type codom :db.type/string)
   :db/cardinality :db.cardinality/one
   :db/index       true})

(defn- schema->datahike-tx
  "Build the initial-tx for create-database from an ACSet schema: the object
   marker, one indexed ref per Hom, and one typed column per Attr."
  [acset-schema]
  (-> (ob-schema-tx)
      (into (mapv hom->schema-tx  (:homs  acset-schema)))
      (into (mapv attr->schema-tx (:attrs acset-schema)))))

(defn- fresh-id-config
  "Disposable in-memory datahike config with a fresh UUID id, so every
   empty ACSet gets its own store and doesn't collide with peers."
  [acset-schema]
  {:store {:backend :memory
           :id (random-uuid)}
   :keep-history? false
   :schema-flexibility :write
   :initial-tx (schema->datahike-tx acset-schema)})

(declare ->DatahikeACSet)

(defrecord DatahikeACSet [schema-data conn]
  a/IACSet
  (-schema [_] schema-data)

  (-nparts [_ ob]
    (count (d/q '[:find ?e :in $ ?ob :where [?e :katzen/ob ?ob]]
                (d/db conn) ob)))

  (-parts [_ ob]
    (sort
     (map first
          (d/q '[:find ?e :in $ ?ob :where [?e :katzen/ob ?ob]]
               (d/db conn) ob))))

  (-subpart [_ mname part-id]
    (ffirst
     (d/q '[:find ?v :in $ ?e ?m :where [?e ?m ?v]]
          (d/db conn) part-id mname)))

  (-subpart-all [_ mname]
    (into {}
          (d/q '[:find ?e ?v :in $ ?m :where [?e ?m ?v]]
               (d/db conn) mname)))

  (-incident [_ mname value]
    (sort
     (map first
          (d/q '[:find ?e :in $ ?m ?v :where [?e ?m ?v]]
               (d/db conn) mname value))))

  (-add-parts [self ob n]
    ;; One transaction with n tempids; the report's :tempids gives us
    ;; the assigned entity ids in insertion order.
    (let [tempids (mapv #(str "p" %) (range n))
          tx-data (mapv (fn [tid] {:db/id tid :katzen/ob ob}) tempids)
          report (d/transact conn {:tx-data tx-data})
          tids   (:tempids report)
          new-ids (mapv #(get tids %) tempids)]
      [self new-ids]))

  (-set-subpart [self mname part-id value]
    (d/transact conn {:tx-data [[:db/add part-id mname value]]})
    self)

  (-rem-part [self ob part-id]
    ;; Retract the entity entirely (also removes its outbound attributes).
    ;; Note: like VectorACSet and Catlab, we do NOT cascade to inbound
    ;; references — the caller must clean those up if needed.
    (d/transact conn {:tx-data [[:db/retractEntity part-id]]})
    self))

(defn datahike-acset
  "Create a DatahikeACSet for a schema.

   1-arity: spins up a fresh in-memory store (two empty acsets don't share
   state) — good for tests and scratch.

   2-arity `(datahike-acset schema conn)`: wrap an EXISTING, caller-managed
   datahike connection (e.g. an app's file-backed, konserve-synced DB). The
   ACSet's schema (object marker + one ref per Hom + one typed column per Attr)
   is transacted onto it — idempotent for attributes that already match. The
   ACSet's morphism names form its own attribute namespace, so it coexists with
   whatever else lives in that DB. This is the integration seam for hosting
   ACSets inside applications (dvergr/simmis) on shared, replicated datahike.

   The returned record wraps the connection; IACSet ops side-effect on it (we
   still return the record from set-subpart/etc. for API parity with
   VectorACSet's persistent semantics)."
  ([schema-data]
   (when-not (a/schema-map? schema-data)
     (throw (ex-info "datahike-acset expects a schema map" {:got schema-data})))
   (let [cfg (fresh-id-config schema-data)]
     (d/create-database cfg)
     (->DatahikeACSet schema-data (d/connect cfg))))
  ([schema-data conn]
   (when-not (a/schema-map? schema-data)
     (throw (ex-info "datahike-acset expects a schema map" {:got schema-data})))
   (d/transact conn {:tx-data (schema->datahike-tx schema-data)})
   (->DatahikeACSet schema-data conn)))

;; Convenience graph constructors mirroring katzen.acset's hand-written ones.

(defn graph
  "Empty graph as a DatahikeACSet."
  []
  (datahike-acset a/SchGraph))
