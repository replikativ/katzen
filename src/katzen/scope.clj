(ns katzen.scope
  "Scope tracking for GATs - provides variable hygiene in theory morphisms.

  Every identifier in a GAT has a scope tag (UUID) that prevents name collisions
  when translating between theories. This implements the same scoping mechanism
  as GATlab.jl's Scopes module.

  Key types:
  - ScopeTag: UUID identifying a scope
  - Ident: (tag, lid, name) triple for identifiers
  - LID: Local identifier (integer position in scope)

  Key operations:
  - retag: Change scope tags (used in theory morphisms)
  - rename: Change names within a scope
  - reident: Replace identifiers entirely"
  (:require [clojure.spec.alpha :as s])
  (:import [java.util UUID]))

;;; ============================================================================
;;; Core Types
;;; ============================================================================

(defrecord ScopeTag [uuid]
  Object
  (toString [_] (str "ScopeTag(" uuid ")")))

(defn scope-tag
  "Create a new scope tag with a random UUID."
  ([]
   (->ScopeTag (UUID/randomUUID)))
  ([uuid]
   (->ScopeTag uuid)))

(defrecord Ident [tag lid name]
  Object
  (toString [_]
    (str "Ident(" tag ", " lid ", " name ")")))

(defn ident
  "Create an identifier with given tag, local-id, and name.

  Parameters:
    tag - ScopeTag identifying which scope this belongs to
    lid - Integer position within the scope
    name - Symbol name (can be nil for anonymous identifiers)"
  [tag lid name]
  {:pre [(instance? ScopeTag tag)
         (integer? lid)
         (or (nil? name) (symbol? name))]}
  (->Ident tag lid name))

(defn gat-ident?
  "Check if value is a GAT Ident."
  [x]
  (instance? Ident x))

(defn scope-tag?
  "Check if value is a ScopeTag."
  [x]
  (instance? ScopeTag x))

;;; ============================================================================
;;; Specs
;;; ============================================================================

(s/def ::uuid uuid?)
(s/def ::scope-tag (s/and scope-tag? #(uuid? (:uuid %))))
(s/def ::lid nat-int?)
(s/def ::name (s/nilable symbol?))
(s/def ::ident (s/and gat-ident?
                      (s/keys :req-un [::scope-tag ::lid ::name])))

;;; ============================================================================
;;; Scoped Protocol
;;; ============================================================================

(defprotocol IScoped
  "Protocol for types that contain scoped identifiers and can be transformed."
  (retag [this tag-map]
    "Replace scope tags according to tag-map: {old-tag new-tag}.

    Used when applying theory morphisms to change the scope of variables.")

  (rename [this tag name-map]
    "Replace names within a specific scope.

    Parameters:
      tag - ScopeTag to rename within
      name-map - Map from old names to new names {old-symbol new-symbol}")

  (reident [this ident-map]
    "Replace identifiers according to ident-map: {old-ident new-ident}.

    Most general transformation - can change tag, lid, and name simultaneously."))

;;; ============================================================================
;;; IScoped Implementation for Core Types
;;; ============================================================================

(extend-protocol IScoped
  Ident
  (retag [this tag-map]
    (if-let [new-tag (get tag-map (:tag this))]
      (assoc this :tag new-tag)
      this))

  (rename [this tag name-map]
    (if (and (= tag (:tag this))
             (:name this)
             (contains? name-map (:name this)))
      (assoc this :name (get name-map (:name this)))
      this))

  (reident [this ident-map]
    (get ident-map this this))

  ;; Recursive structures
  clojure.lang.IPersistentVector
  (retag [this tag-map]
    (mapv #(retag % tag-map) this))

  (rename [this tag name-map]
    (mapv #(rename % tag name-map) this))

  (reident [this ident-map]
    (mapv #(reident % ident-map) this))

  clojure.lang.IPersistentMap
  (retag [this tag-map]
    (into {}
          (map (fn [[k v]]
                 [(retag k tag-map) (retag v tag-map)]))
          this))

  (rename [this tag name-map]
    (into {}
          (map (fn [[k v]]
                 [(rename k tag name-map) (rename v tag name-map)]))
          this))

  (reident [this ident-map]
    (into {}
          (map (fn [[k v]]
                 [(reident k ident-map) (reident v ident-map)]))
          this))

  clojure.lang.IPersistentList
  (retag [this tag-map]
    (map #(retag % tag-map) this))

  (rename [this tag name-map]
    (map #(rename % tag name-map) this))

  (reident [this ident-map]
    (map #(reident % ident-map) this))

  clojure.lang.IPersistentSet
  (retag [this tag-map]
    (into #{} (map #(retag % tag-map)) this))

  (rename [this tag name-map]
    (into #{} (map #(rename % tag name-map)) this))

  (reident [this ident-map]
    (into #{} (map #(reident % ident-map)) this))

  ;; Base case: non-scoped values return unchanged
  Object
  (retag [this _] this)
  (rename [this _ _] this)
  (reident [this _] this)

  nil
  (retag [this _] this)
  (rename [this _ _] this)
  (reident [this _] this))

;;; ============================================================================
;;; Scope Context Management
;;; ============================================================================

(defrecord ScopeContext [tag next-lid bindings]
  Object
  (toString [_]
    (str "ScopeContext(" tag ", next-lid=" next-lid ", bindings=" bindings ")")))

(defn scope-context
  "Create a new scope context with a fresh tag.

  A scope context tracks:
  - tag: The ScopeTag for this scope
  - next-lid: Next available local identifier
  - bindings: Map from names to Idents"
  ([]
   (scope-context (scope-tag)))
  ([tag]
   (->ScopeContext tag 0 {})))

(defn bind
  "Bind a name in the scope context, returning [new-context ident].

  Creates a fresh Ident with the next available lid."
  [ctx name]
  {:pre [(instance? ScopeContext ctx)
         (symbol? name)]}
  (let [new-lid (:next-lid ctx)
        new-ident (ident (:tag ctx) new-lid name)
        new-bindings (assoc (:bindings ctx) name new-ident)
        new-ctx (-> ctx
                    (assoc :next-lid (inc new-lid))
                    (assoc :bindings new-bindings))]
    [new-ctx new-ident]))

(defn bind-many
  "Bind multiple names in sequence, returning [new-context idents].

  Example:
    (bind-many ctx ['a 'b 'c])
    => [new-ctx [ident-a ident-b ident-c]]"
  [ctx names]
  (reduce
   (fn [[ctx idents] name]
     (let [[new-ctx new-ident] (bind ctx name)]
       [new-ctx (conj idents new-ident)]))
   [ctx []]
   names))

(defn lookup
  "Look up a name in the scope context, returning the Ident or nil."
  [ctx name]
  (get-in ctx [:bindings name]))

(defn has-binding?
  "Check if a name is bound in the scope context."
  [ctx name]
  (contains? (:bindings ctx) name))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn fresh-tag
  "Create a fresh scope tag."
  []
  (scope-tag))

(defn merge-tag-maps
  "Merge multiple tag maps, checking for conflicts."
  [& tag-maps]
  (apply merge tag-maps))

(defn collect-tags
  "Collect all scope tags appearing in a scoped term."
  [term]
  (let [tags (atom #{})]
    (letfn [(collect [x]
              (cond
                (gat-ident? x)
                (swap! tags conj (:tag x))

                (map? x)
                (doseq [[k v] x]
                  (collect k)
                  (collect v))

                (coll? x)
                (doseq [item x]
                  (collect item))))]
      (collect term))
    @tags))

(defn alpha-equivalent?
  "Check if two terms are alpha-equivalent (same up to scope tag renaming).

  Two terms are alpha-equivalent if they have the same structure and
  corresponding identifiers have the same lid and name, even if scope tags differ."
  [term1 term2]
  (cond
    (and (gat-ident? term1) (gat-ident? term2))
    (and (= (:lid term1) (:lid term2))
         (= (:name term1) (:name term2)))

    (and (map? term1) (map? term2))
    (and (= (count term1) (count term2))
         (every? (fn [[k1 v1]]
                   (when-let [v2 (get term2 k1)]
                     (alpha-equivalent? v1 v2)))
                 term1))

    (and (coll? term1) (coll? term2))
    (and (= (count term1) (count term2))
         (every? identity
                 (map alpha-equivalent? term1 term2)))

    :else
    (= term1 term2)))

;;; ============================================================================
;;; Advanced Scoping - Multi-level Contexts
;;; ============================================================================

(defrecord ScopeList [scopes]
  Object
  (toString [_]
    (str "ScopeList(" (count scopes) " scopes)")))

(defn scope-list
  "Create a multi-level scope list.

  Scopes are ordered from most recent (index 0) to oldest.
  Each scope must have a unique tag."
  [scopes]
  (let [tags (map :tag scopes)]
    (when-not (= (count tags) (count (set tags)))
      (throw (ex-info "ScopeList cannot have duplicate tags"
                      {:tags tags}))))
  (->ScopeList (vec scopes)))

(defn scope-list?
  "Check if value is a ScopeList."
  [x]
  (instance? ScopeList x))

(defrecord AppendContext [base new-scope]
  Object
  (toString [_]
    (str "AppendContext(...)")))

(defrecord EmptyContext []
  Object
  (toString [_] "EmptyContext"))

(defn empty-context?
  "Check if a context is empty."
  [ctx]
  (instance? EmptyContext ctx))

(defn nscopes
  "Return the number of scopes in a context."
  [ctx]
  (cond
    (empty-context? ctx) 0
    (instance? ScopeContext ctx) 1
    (scope-list? ctx) (count (:scopes ctx))
    (instance? AppendContext ctx) (+ (nscopes (:base ctx)) 1)
    :else 0))

(defn getscope
  "Get the scope at a given level (1-indexed).

  Level 1 is the most recent scope, level 2 is next, etc."
  [ctx level]
  {:pre [(pos? level)]}
  (cond
    (instance? ScopeContext ctx)
    (if (= level 1)
      ctx
      (throw (ex-info "Level out of bounds" {:level level :max 1})))

    (scope-list? ctx)
    (if (<= level (count (:scopes ctx)))
      (nth (:scopes ctx) (dec level))
      (throw (ex-info "Level out of bounds" {:level level :max (count (:scopes ctx))})))

    (instance? AppendContext ctx)
    (let [base-count (nscopes (:base ctx))]
      (if (> level base-count)
        (:new-scope ctx)
        (getscope (:base ctx) level)))

    :else
    (throw (ex-info "Cannot get scope from this context" {:ctx ctx}))))

(defn getlevel
  "Find the level of a tag or name in a context.

  Returns the level (1-indexed) where the tag/name is found (most recent = 1)."
  [ctx query]
  (cond
    ;; Query by tag
    (instance? ScopeTag query)
    (cond
      (instance? ScopeContext ctx)
      (if (= query (:tag ctx)) 1
          (throw (ex-info "Tag not found" {:tag query})))

      (scope-list? ctx)
      (if-let [idx (first (keep-indexed
                           (fn [i sc] (when (= query (:tag sc)) i))
                           (:scopes ctx)))]
        (inc idx)
        (throw (ex-info "Tag not found" {:tag query})))

      (instance? AppendContext ctx)
      (if (= query (:tag (:new-scope ctx)))
        (inc (nscopes (:base ctx)))
        (getlevel (:base ctx) query))

      :else
      (throw (ex-info "Cannot query this context" {:ctx ctx})))

    ;; Query by name
    (symbol? query)
    (cond
      (instance? ScopeContext ctx)
      (if (contains? (:bindings ctx) query) 1
          (throw (ex-info "Name not found" {:name query})))

      (scope-list? ctx)
      ;; Search from most recent to oldest
      (if-let [idx (first (keep-indexed
                           (fn [i sc]
                             (when (contains? (:bindings sc) query) i))
                           (:scopes ctx)))]
        (inc idx)
        (throw (ex-info "Name not found" {:name query})))

      (instance? AppendContext ctx)
      (if (contains? (:bindings (:new-scope ctx)) query)
        (inc (nscopes (:base ctx)))
        (getlevel (:base ctx) query))

      :else
      (throw (ex-info "Cannot query this context" {:ctx ctx})))

    :else
    (throw (ex-info "Invalid query for getlevel" {:query query}))))

(defn hastag?
  "Check if a tag exists in the context."
  [ctx tag]
  (try
    (getlevel ctx tag)
    true
    (catch Exception _ false)))

(defn hasname?
  "Check if a name exists in the context."
  [ctx name]
  (try
    (getlevel ctx name)
    true
    (catch Exception _ false)))

(defn lookup-multi
  "Lookup a name in a multi-level context.

  Returns the ident from the most recent scope that has this name."
  [ctx name]
  (cond
    (instance? ScopeContext ctx)
    (lookup ctx name)

    (scope-list? ctx)
    ;; Search from most recent (index 0) to oldest
    (some #(lookup % name) (:scopes ctx))

    (instance? AppendContext ctx)
    (or (lookup (:new-scope ctx) name)
        (lookup-multi (:base ctx) name))

    :else
    nil))

(defn append-context
  "Append a new scope to a context.

  The new scope becomes the most recent scope (level N+1)."
  [base new-scope]
  (when (hastag? base (:tag new-scope))
    (throw (ex-info "Cannot append scope with duplicate tag"
                    {:tag (:tag new-scope)})))
  (->AppendContext base new-scope))

(defn getidents
  "Get all idents from all scopes in a context."
  [ctx]
  (cond
    (instance? ScopeContext ctx)
    (vals (:bindings ctx))

    (scope-list? ctx)
    (mapcat #(vals (:bindings %)) (:scopes ctx))

    (instance? AppendContext ctx)
    (concat (getidents (:base ctx))
            (vals (:bindings (:new-scope ctx))))

    :else
    []))

(defn empty-context
  "Create an empty context with no scopes."
  []
  (->EmptyContext))
