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

(defn- db-cardinality
  "Map a morphism's `:cardinality` (`:one` default, or `:many`) to datahike's
   native cardinality. A `:many` morphism is a RELATION (not a function): it
   uses datahike's native `:db.cardinality/many` rather than a junction object —
   katzen as a lens over what datahike already does. Reified relations (with
   their own attributes) stay modeled as their own object."
  [m]
  (case (:cardinality m :one)
    :many :db.cardinality/many
    :db.cardinality/one))

(defn- hom->schema-tx
  "Datahike schema attribute for a Hom morphism. Indexed for incident lookups;
   honours `:cardinality` (:one/:many) and an optional `:unique`
   (:db.unique/identity | :db.unique/value)."
  [{:keys [name unique] :as h}]
  (cond-> {:db/ident       name
           :db/valueType   :db.type/ref
           :db/cardinality (db-cardinality h)
           :db/index       true}
    unique (assoc :db/unique unique)))

(def ^:private attr-type->value-type
  "Map an ACSet attr-type name to a datahike :db/valueType. Covers the common
   scalar attr-types (Catlab's are Julia types like Symbol/String/Int/Float);
   unknown attr-types fall back to :db.type/string."
  {:String  :db.type/string  :Str    :db.type/string
   ;; Identity / URI: the cross-ACSet reference space (see katzen.xref). STRING-
   ;; valued (e.g. "demo.core/a") — qualified names as URIs. Strings are the
   ;; portable identity space: language-agnostic, addressable, and unaffected by
   ;; the released-datahike symbol-value query bug (fixed on the datahike branch
   ;; fix/symbol-value-scalar-input, but strings work everywhere regardless).
   :Identity :db.type/string :URI    :db.type/string
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
  [{:keys [name codom unique] :as a}]
  (cond-> {:db/ident       name
           :db/valueType   (get attr-type->value-type codom :db.type/string)
           :db/cardinality (db-cardinality a)
           :db/index       true}
    unique (assoc :db/unique unique)))

(defn- many?
  "True if morphism `mname` is declared `:cardinality :many` in `schema-data`."
  [schema-data mname]
  (= :many (:cardinality (second (a/morphism-by-name schema-data mname)))))

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
    (let [vs (map first (d/q '[:find ?v :in $ ?e ?m :where [?e ?m ?v]]
                             (d/db conn) part-id mname))]
      (if (many? schema-data mname) (set vs) (first vs))))

  (-subpart-all [_ mname]
    (let [rows (d/q '[:find ?e ?v :in $ ?m :where [?e ?m ?v]]
                    (d/db conn) mname)]
      (if (many? schema-data mname)
        (reduce (fn [acc [e v]] (update acc e (fnil conj #{}) v)) {} rows)
        (into {} rows))))

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
    ;; For a :many morphism, `value` is a COLLECTION and we REPLACE the current
    ;; set (retract existing, add each) — datahike-native many, no junction. For
    ;; :one, set the single value.
    (if (many? schema-data mname)
      (let [existing (map first (d/q '[:find ?v :in $ ?e ?m :where [?e ?m ?v]]
                                     (d/db conn) part-id mname))]
        (d/transact conn {:tx-data (into (mapv (fn [v] [:db/retract part-id mname v]) existing)
                                         (mapv (fn [v] [:db/add part-id mname v]) value))}))
      (d/transact conn {:tx-data [[:db/add part-id mname value]]}))
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
   ;; Install only the idents this conn does NOT already have. Hosting an ACSet
   ;; inside an existing app DB, the schema's columns (e.g. :entity/title,
   ;; :entity/employer) typically already exist — possibly with flags this
   ;; backend wouldn't set (e.g. :db.unique/value) or WITHOUT ones it would
   ;; (e.g. :db/index). Re-asserting them is at best a no-op and at worst a
   ;; hard datahike error ("Update not supported for these schema attributes",
   ;; e.g. adding :db/index to an existing attr). So we defer to whatever the
   ;; existing schema says and only add the genuinely missing idents (commonly
   ;; just the :katzen/ob object marker).
   (let [existing (into #{} (map first)
                        (d/q '[:find ?id :where [?e :db/ident ?id]] (d/db conn)))
         tx (into [] (remove #(contains? existing (:db/ident %)))
                  (schema->datahike-tx schema-data))]
     (when (seq tx)
       (d/transact conn {:tx-data tx})))
   (->DatahikeACSet schema-data conn)))

;; Convenience graph constructors mirroring katzen.acset's hand-written ones.

(defn graph
  "Empty graph as a DatahikeACSet."
  []
  (datahike-acset a/SchGraph))
