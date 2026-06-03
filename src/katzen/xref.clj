(ns katzen.xref
  "Cross-ACSet references: relate parts of two ACSets that live in SEPARATE
   databases by a *shared identity AttrType*, rather than by an internal Hom.

   Why this exists. A Hom `m : Ob → Ob'` is an INTERNAL reference — its target
   is a part-id, meaningful only inside one ACSet/one database. A reference that
   must cross a database boundary (e.g. a knowledge-base note pointing at a code
   definition, or a fork pointing at its parent) cannot be a Hom: the part-id is
   meaningless on the other side. It must instead be an Attr valued in a shared
   AttrType — an `Identity` (a qualified name / URI, a fixed value space both
   schemas agree on).

   Resolving such a cross-reference is the PULLBACK (fiber product) over the
   shared AttrType:

         A ×_Id B
          ╱        ╲
        A ─a-attr─▶ Id ◀─b-attr─ B

   i.e. the set of pairs (pa, pb) whose identity values coincide. This is the
   limit of the cospan  ObA --a-attr--> Id <--b-attr-- ObB.

   `xref` computes it via the IACSet protocol's `subpart-all` + `incident`
   (an indexed inverse-image lookup), so it is schema-agnostic and works
   uniformly over Vector- and datahike-backed ACSets — the standardized
   cross-reference primitive."
  (:require [katzen.acset :as a]))

(defn- attr-codom
  "The codomain (an AttrType name) of attr `attr-name` in `schema`, or nil if it
   isn't an Attr of the schema."
  [schema attr-name]
  (some #(when (= attr-name (:name %)) (:codom %)) (:attrs schema)))

(defn shared-attr-type
  "The AttrType the two attrs share, or nil if they don't agree. A non-nil
   result is the precondition for a meaningful cross-reference: both sides must
   be valued in the same identity space."
  [acset-a a-attr acset-b b-attr]
  (let [ta (attr-codom (a/schema acset-a) a-attr)
        tb (attr-codom (a/schema acset-b) b-attr)]
    (when (and ta (= ta tb)) ta)))

(defn xref
  "Pullback of `acset-a` and `acset-b` over the identity values of attrs
   `a-attr` and `b-attr` (which must share an AttrType).

   Returns a vector of matches `{:a <part-id in A> :b <part-id in B> :id <value>}`
   — every (pa, pb) with `a-attr(pa) = b-attr(pb)`. The pair is the categorical
   cross-reference; `:id` is the shared identity value that links them.

   Throws if the two attrs are not valued in the same AttrType, since a
   cross-reference across different identity spaces is not well-defined."
  [acset-a a-attr acset-b b-attr]
  (when-not (shared-attr-type acset-a a-attr acset-b b-attr)
    (throw (ex-info "xref attrs must share an AttrType"
                    {:a-attr a-attr :b-attr b-attr})))
  (vec
   (for [[pa v] (a/subpart-all acset-a a-attr)
         pb     (a/incident acset-b b-attr v)]
     {:a pa :b pb :id v})))

(defn dangling
  "The parts of `acset-a` whose `a-attr` identity value has NO match under
   `b-attr` in `acset-b` — i.e. cross-references that don't resolve (a broken
   link, the analogue of a Logseq link to a missing page). Returns
   `[{:a <part-id> :id <value>} …]`.

   Referential integrity across databases is thus a QUERY (compute `dangling`),
   not an enforced foreign key — appropriate when the two sides are derived/
   re-indexed independently (e.g. code re-projected from text)."
  [acset-a a-attr acset-b b-attr]
  (when-not (shared-attr-type acset-a a-attr acset-b b-attr)
    (throw (ex-info "dangling attrs must share an AttrType"
                    {:a-attr a-attr :b-attr b-attr})))
  (vec
   (for [[pa v] (a/subpart-all acset-a a-attr)
         :when  (empty? (a/incident acset-b b-attr v))]
     {:a pa :id v})))
