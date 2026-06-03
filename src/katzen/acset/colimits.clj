(ns katzen.acset.colimits
  "Colimits of ACSets — `coproduct` and `pushout`.

   katzen had (co)limits only in FinSet; composition patterns that glue
   ACSets (structured cospans, gluing graphs, the species-identification
   in `petri/compose-petri`) had to hand-roll a coproduct + coequalizer.
   This computes them generically, object-by-object, through the FinSet
   colimits, reindexing the homs/attrs.

   Construction (the standard pointwise colimit of a C-Set):
   - **coproduct** X ⊔ Y — per object, disjoint union of parts; a hom/attr
     keeps each side's value, retargeted into the union.
   - **pushout** of X ←f– A –g→ Y — per object, quotient X ⊔ Y by
     identifying f(a) with g(a) for every part a of the apex A (a FinSet
     coequalizer); homs are reindexed through the quotient projection.

   Each returns `{:type … :apex <acset> :legs [ιX ιY]}` mirroring the
   FinSet colimit shape, the legs being `ACSetMorphism`s into the apex.

   Part ids are treated as opaque (the vector backend numbers from 1, and
   parts need not be contiguous after deletion): every mapping is explicit,
   built from `(parts …)` rather than from id arithmetic. Attributes on
   identified parts are assumed consistent — a merged class takes a
   representative value."
  (:require [katzen.acset :as a]
            [katzen.acset.morphism :as am]
            [katzen.finset :as fs]
            [katzen.finset.colimits :as colim]))

;; ============================================================================
;; Coproduct
;; ============================================================================

(defn coproduct
  "Coproduct (disjoint union) X ⊔ Y of two ACSets on the same schema.
   Returns {:type :coproduct :apex R :legs [ιX ιY]}."
  [X Y]
  (let [schema (a/schema X)
        objs   (:objects schema)
        ;; allocate parts in R; xmap/ymap : source-part → apex-part, per object
        [R xmap ymap]
        (reduce (fn [[r xm ym] O]
                  (let [xs (vec (a/parts X O))
                        ys (vec (a/parts Y O))
                        [r ids] (a/add-parts r O (+ (count xs) (count ys)))
                        ids (vec ids)]
                    [r
                     (assoc xm O (zipmap xs (subvec ids 0 (count xs))))
                     (assoc ym O (zipmap ys (subvec ids (count xs))))]))
                [(a/vector-acset schema) {} {}] objs)
        side (fn [r m src {:keys [name dom codom]} hom?]
               (reduce (fn [r p]
                         (a/set-subpart r name (get-in m [dom p])
                                        (if hom?
                                          (get-in m [codom (a/subpart src name p)])
                                          (a/subpart src name p))))
                       r (a/parts src dom)))
        R (reduce (fn [r h] (-> r (side xmap X h true) (side ymap Y h true)))
                  R (:homs schema))
        R (reduce (fn [r at] (-> r (side xmap X at false) (side ymap Y at false)))
                  R (:attrs schema))
        ιX (am/->ACSetMorphism X R (into {} (for [O objs] [O (get xmap O)])))
        ιY (am/->ACSetMorphism Y R (into {} (for [O objs] [O (get ymap O)])))]
    {:type :coproduct :apex R :legs [ιX ιY]}))

;; ============================================================================
;; Pushout
;; ============================================================================

(defn pushout
  "Pushout of the span X ←f– A –g→ Y (ACSet morphisms `f`, `g` sharing
   apex `A`). Returns {:type :pushout :apex R :legs [ιX ιY]}, identifying
   f(a) with g(a) for every part a of A."
  [f g]
  (let [A      (:src f)
        X      (:tgt f)
        Y      (:tgt g)
        schema (a/schema X)
        objs   (:objects schema)
        ;; per object: a FinSet coequalizer of (A → X⊔Y via f) and (via g),
        ;; in a 0-based index space [0..nx) ∪ [nx..nx+ny)
        per
        (into {}
              (for [O objs]
                (let [xs    (vec (a/parts X O))
                      ys    (vec (a/parts Y O))
                      nx    (count xs)
                      total (+ nx (count ys))
                      x-idx (zipmap xs (range))                      ; X-part → 0..nx-1
                      y-idx (zipmap ys (map #(+ nx %) (range)))      ; Y-part → nx..
                      as    (vec (a/parts A O))
                      coeq  (when (seq as)
                              (colim/coequalizer
                               (fs/fin-function (mapv #(x-idx (get-in f [:components O %])) as) total)
                               (fs/fin-function (mapv #(y-idx (get-in g [:components O %])) as) total)))
                      proj  (if coeq (first (:legs coeq)) (fs/id-function total))
                      ncls  (if coeq (fs/cardinality (:apex coeq)) total)]
                  [O {:x-idx x-idx :y-idx y-idx :proj proj :ncls ncls}])))
        ;; allocate apex parts; class index (0-based) → apex part id
        [R cls->part]
        (reduce (fn [[r acc] O]
                  (let [[r ids] (a/add-parts r O (:ncls (per O)))]
                    [r (assoc acc O (vec ids))]))
                [(a/vector-acset schema) {}] objs)
        x->apex (fn [O p] (nth (cls->part O) (fs/app (:proj (per O)) ((:x-idx (per O)) p))))
        y->apex (fn [O q] (nth (cls->part O) (fs/app (:proj (per O)) ((:y-idx (per O)) q))))
        side (fn [r ->apex src {:keys [name dom codom]} hom?]
               (reduce (fn [r p]
                         (a/set-subpart r name (->apex dom p)
                                        (if hom?
                                          (->apex codom (a/subpart src name p))
                                          (a/subpart src name p))))
                       r (a/parts src dom)))
        R (reduce (fn [r h]  (-> r (side x->apex X h true)  (side y->apex Y h true)))  R (:homs schema))
        R (reduce (fn [r at] (-> r (side x->apex X at false) (side y->apex Y at false))) R (:attrs schema))
        ιX (am/->ACSetMorphism X R (into {} (for [O objs] [O (into {} (for [p (a/parts X O)] [p (x->apex O p)]))])))
        ιY (am/->ACSetMorphism Y R (into {} (for [O objs] [O (into {} (for [q (a/parts Y O)] [q (y->apex O q)]))])))]
    {:type :pushout :apex R :legs [ιX ιY]}))
