(ns katzen.aggregate
  "Rollup / aggregation over an ACSet as a fold with a commutative MONOID — the
   bounded model the ACT research recommends (Spivak's *Functorial Aggregation*
   common case: aggregation valued in a commutative monoid `(R, ⊛)`, ~95% of
   Notion-style rollups: sum / count / min / max / and / or). The full
   polynomial-functor theory is intentionally out of scope.

   A monoid is `{:empty e :combine f}` with `f` associative+commutative and `e`
   its unit. A rollup folds the values reachable from a part — typically the
   inverse image of a junction hom (a fan-in) — into one aggregate."
  (:require [katzen.acset :as a]
            [katzen.eval :as ev]))

(def monoids
  "Standard commutative monoids for rollups. `:combine` takes (acc value)."
  {:sum    {:empty 0     :combine +}
   :count  {:empty 0     :combine (fn [acc _] (inc acc))}
   :min    {:empty nil   :combine (fn [acc v] (if (nil? acc) v (min acc v)))}
   :max    {:empty nil   :combine (fn [acc v] (if (nil? acc) v (max acc v)))}
   :and    {:empty true  :combine (fn [acc v] (and acc v))}
   :or     {:empty false :combine (fn [acc v] (boolean (or acc v)))}
   :concat {:empty ""    :combine str}
   :into   {:empty []    :combine conj}})

(defn fold
  "Fold `values` with `monoid` (a `{:empty :combine}` map or a key into
   `monoids`). The categorical aggregate of a multiset of values."
  [monoid values]
  (let [{:keys [empty combine]} (if (keyword? monoid) (monoids monoid) monoid)]
    (reduce combine empty values)))

(defn rollup
  "Aggregate over the parts related to `part` by the INVERSE IMAGE of junction
   hom `via` (the parts whose `via` = part — a fan-in), mapping each to a value
   with `value-fn` (part → value) and folding with `monoid`.

   E.g. sum of `:item/amount` over the line-items pointing at an invoice:
     (rollup acset :sum :item/invoice #(a/subpart acset :item/amount %) inv)"
  [acset monoid via value-fn part]
  (fold monoid (map value-fn (a/incident acset via part))))

(defn rollup-attr
  "Convenience rollup: fold the values of attr/hom `attr` over the fan-in of
   junction hom `via` at `part`. (= `rollup` with value-fn = subpart of attr.)"
  [acset monoid via attr part]
  (rollup acset monoid via #(a/subpart acset attr %) part))

(defn rollup-term
  "Rollup where each related part's value is a computed `katzen.eval` term
   (`{:var? :term}`, var bound to the related part) — the general case: aggregate
   a derived quantity, not just a stored column."
  ([acset monoid via prop part] (rollup-term acset ev/base-model monoid via prop part))
  ([acset model monoid via prop part]
   (rollup acset monoid via #(ev/derived acset model prop %) part)))
