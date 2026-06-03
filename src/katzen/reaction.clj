(ns katzen.reaction
  "Reaction networks: a generalization of Petri nets where each reaction
   carries a *rate law* — an expression for how its rate depends on the
   state vector. Mass-action is one rate law among several; Michaelis-
   Menten, Hill, and user-supplied symbolic expressions are others.

   This is a direct analogue of Catalyst.jl's `@reaction_network`: same
   underlying stoichiometric structure as a Petri net, but the rate
   isn't forced to be mass-action.

   The schema:

     SchReactionNetwork:
       :S Species, :R Reaction, :Sub substrate-arcs, :Pr product-arcs
       :ss : Sub → S    substrate species
       :sr : Sub → R    substrate reaction
       :ps : Pr  → S    product species
       :pr : Pr  → R    product reaction

   Stoichiometric multiplicity is handled the same way as Petri nets —
   N arcs from species X to reaction r means X is consumed N times by r.

   Rate laws are stored OUTSIDE the ACSet as a map {reaction-id → law}.
   A law is itself a small map:

     {:type :mass-action :k 0.1}
     {:type :michaelis-menten :Vmax 1.0 :Km 0.5 :substrate species-id}
     {:type :hill            :Vmax 1.0 :Km 0.5 :n 2 :substrate species-id}
     {:type :expr  :form '(+ (* 0.5 S I) (* 0.1 R))
                   :bindings {'S species-id-S, 'I species-id-I, 'R species-id-R}}

   For :expr, the `:bindings` map names readable symbols and points each
   at a species id; the compiler replaces every occurrence of those
   symbols in `:form` with `(aget u slot)` where `slot` is the species'
   index under the current layout. The form is *symbolic* — not a
   captured Clojure closure — so the raster ftm body stays straight-line
   and primitive-fast.

   `ReactionDynamics` bundles a network with its rate-law map and
   implements `katzen.compile.core/RasterCompilable`. The protocol's
   driver handles compilation."
  (:require [clojure.walk :as walk]
            [katzen.acset :as a]
            [katzen.compile.core :as cc]))

;; ============================================================================
;; SchReactionNetwork
;; ============================================================================

(def SchReactionNetwork
  "Schema of reaction networks: species, reactions, and the substrate /
   product arcs that determine stoichiometry."
  {:name       'SchReactionNetwork
   :objects    [:S :R :Sub :Pr]
   :homs       [{:name :ss :dom :Sub :codom :S}
                {:name :sr :dom :Sub :codom :R}
                {:name :ps :dom :Pr  :codom :S}
                {:name :pr :dom :Pr  :codom :R}]
   :attr-types []
   :attrs      []})

;; ============================================================================
;; Constructors
;; ============================================================================

(defn reaction-network
  "Empty reaction network."
  []
  (a/vector-acset SchReactionNetwork))

(defn add-species
  "Returns [new-net s-id]."
  [n] (a/add-part n :S))

(defn add-reaction
  "Returns [new-net r-id]."
  [n] (a/add-part n :R))

(defn add-substrate
  "Add a substrate arc: species `s` is a substrate of reaction `r`.
   Repeat for multiplicity > 1."
  [n s r]
  (let [[n sub] (a/add-part n :Sub)]
    [(-> n (a/set-subpart :ss sub s) (a/set-subpart :sr sub r)) sub]))

(defn add-product
  "Add a product arc: species `s` is produced by reaction `r`.
   Repeat for multiplicity > 1."
  [n s r]
  (let [[n pr] (a/add-part n :Pr)]
    [(-> n (a/set-subpart :ps pr s) (a/set-subpart :pr pr r)) pr]))

;; ============================================================================
;; Accessors
;; ============================================================================

(defn species [n] (vec (a/parts n :S)))
(defn reactions [n] (vec (a/parts n :R)))

(defn substrate-multiplicity
  "{reaction → {species → count}}"
  [n]
  (reduce (fn [acc sub]
            (let [s (a/subpart n :ss sub)
                  r (a/subpart n :sr sub)]
              (update-in acc [r s] (fnil inc 0))))
          {}
          (a/parts n :Sub)))

(defn product-multiplicity
  "{reaction → {species → count}}"
  [n]
  (reduce (fn [acc pr]
            (let [s (a/subpart n :ps pr)
                  r (a/subpart n :pr pr)]
              (update-in acc [r s] (fnil inc 0))))
          {}
          (a/parts n :Pr)))

(defn natural-layout
  "Layout assigning slots 0..k-1 to species in `(species n)` order."
  [n]
  (cc/state-layout (species n)))

;; ============================================================================
;; Rate-law compilation — raster forms and Clojure fns
;; ============================================================================
;;
;; Each rate-law type emits both a raster source form (for the ftm body)
;; and a Clojure rate function (state-vector → double). The two share
;; the same layout so they target the same state slots.

(defn- aget-slot [slot]
  `(raster.arrays/aget ~'u ~slot))

(defn- factor [slot ^long mult]
  (case mult
    1 (aget-slot slot)
    `(Math/pow ~(aget-slot slot) ~mult)))

(defn- mass-action-rate-form
  "Source form: k * ∏ X^mult over substrates."
  [k substrate-slot-mult]
  (if (empty? substrate-slot-mult)
    (double k)
    (reduce (fn [acc [s m]] `(raster.numeric/* ~acc ~(factor s m)))
            (double k)
            substrate-slot-mult)))

(defn- mass-action-rate-clj
  "Returns (fn [u] rate-double)."
  [k substrate-slot-mult]
  (fn [^doubles u]
    (reduce
     (fn [acc [^long s ^long m]]
       (* acc (Math/pow (aget u s) m)))
     (double k)
     substrate-slot-mult)))

(defn- mm-rate-form
  "Michaelis-Menten: Vmax · X / (Km + X)."
  [Vmax Km slot]
  (let [X (aget-slot slot)]
    `(raster.numeric// (raster.numeric/* ~(double Vmax) ~X)
                       (raster.numeric/+ ~(double Km) ~X))))

(defn- mm-rate-clj [Vmax Km slot]
  (fn [^doubles u]
    (let [X (aget u slot)]
      (/ (* (double Vmax) X) (+ (double Km) X)))))

(defn- hill-rate-form
  "Hill: Vmax · X^n / (Km^n + X^n)."
  [Vmax Km n slot]
  (let [X (aget-slot slot)
        Xn `(Math/pow ~X ~n)
        Kn (Math/pow (double Km) (long n))]
    `(raster.numeric// (raster.numeric/* ~(double Vmax) ~Xn)
                       (raster.numeric/+ ~Kn ~Xn))))

(defn- hill-rate-clj [Vmax Km n slot]
  (let [Kn (Math/pow (double Km) (long n))]
    (fn [^doubles u]
      (let [X  (aget u slot)
            Xn (Math/pow X (long n))]
        (/ (* (double Vmax) Xn) (+ Kn Xn))))))

(defn- expr-rate-form
  "Symbolic rate expression with explicit symbol → species-id `bindings`.
   For each (sym → species-id) entry, every occurrence of `sym` in
   `form` is replaced with `(aget u slot)` where slot is the species'
   index under `layout`. Other symbols pass through unchanged."
  [form layout bindings]
  (let [sym->aget (into {}
                        (for [[sym sp] bindings]
                          [sym (aget-slot (cc/slot layout sp))]))]
    (walk/postwalk
     (fn [x] (if (and (symbol? x) (contains? sym->aget x))
               (get sym->aget x)
               x))
     form)))

(defn- expr-rate-clj
  "Clojure version: substitute the same way, then compile via eval to a
   function (fn [u] ...). The eval cost happens once at build time."
  [form layout bindings]
  (let [substituted (expr-rate-form form layout bindings)]
    (eval `(fn [^doubles ~'u] ~substituted))))

(defn- rate-emit
  "Dispatch on `:type` of `law`; return [form clj-fn] for the rate
   under the given `layout`. `subs-slot-mult` is the substrate slot/
   multiplicity pairs (used by mass-action only)."
  [law layout subs-slot-mult]
  (case (:type law)
    :mass-action
    [(mass-action-rate-form (:k law) subs-slot-mult)
     (mass-action-rate-clj  (:k law) subs-slot-mult)]

    :michaelis-menten
    (let [slot (cc/slot layout (:substrate law))]
      [(mm-rate-form (:Vmax law) (:Km law) slot)
       (mm-rate-clj  (:Vmax law) (:Km law) slot)])

    :hill
    (let [slot (cc/slot layout (:substrate law))]
      [(hill-rate-form (:Vmax law) (:Km law) (:n law) slot)
       (hill-rate-clj  (:Vmax law) (:Km law) (:n law) slot)])

    :expr
    [(expr-rate-form (:form law) layout (:bindings law {}))
     (expr-rate-clj  (:form law) layout (:bindings law {}))]

    (throw (ex-info "Unknown rate-law type" {:law law}))))

;; ============================================================================
;; Reaction-block emission (analogue of petri/transition-block)
;; ============================================================================

(defn- reaction-block
  "Per-reaction source block: compute the rate, apply stoichiometric
   updates to du."
  [rate-expr in-pairs out-pairs]
  (let [rate-sym (gensym "rate")
        ;; Compute net delta-per-species: out - in. We aggregate so each
        ;; species sees one aset instead of two.
        all-species (into #{} (concat (map first in-pairs) (map first out-pairs)))
        in-by-s  (into {} in-pairs)
        out-by-s (into {} out-pairs)
        deltas (for [s (sort all-species)
                     :let [delta (- (long (get out-by-s s 0))
                                    (long (get in-by-s  s 0)))]
                     :when (not (zero? delta))]
                 [s delta])]
    `(let [~rate-sym ~rate-expr]
       ~@(for [[s ^long delta] deltas]
           `(raster.arrays/aset ~'du ~s
                                (raster.numeric/+ (raster.arrays/aget ~'du ~s)
                                                  (raster.numeric/* ~rate-sym ~(double delta))))))))

(defn- reaction-step-clj
  "Per-reaction Clojure step: compute the rate, apply stoichiometric
   updates. Returns a fn (fn [^doubles du ^doubles u t])."
  [rate-fn in-pairs out-pairs]
  (let [all-species (into #{} (concat (map first in-pairs) (map first out-pairs)))
        in-by-s  (into {} in-pairs)
        out-by-s (into {} out-pairs)
        deltas (vec
                (for [s (sort all-species)
                      :let [delta (- (long (get out-by-s s 0))
                                     (long (get in-by-s  s 0)))]
                      :when (not (zero? delta))]
                  [(long s) (double delta)]))]
    (fn step [^doubles du ^doubles u t]
      (let [rate (double (rate-fn u))]
        (doseq [[^long s ^double delta] deltas]
          (aset du s (+ (aget du s) (* rate delta))))))))

;; ============================================================================
;; Body emission — public entry points
;; ============================================================================

(defn- precompute-rxn [net layout]
  (let [in  (substrate-multiplicity net)
        out (product-multiplicity net)
        slot-of #(cc/slot layout %)]
    {:rs (reactions net)
     :in (into {} (for [r (reactions net)]
                    [r (mapv (fn [[s m]] [(slot-of s) (long m)]) (get in r {}))]))
     :out (into {} (for [r (reactions net)]
                     [r (mapv (fn [[s m]] [(slot-of s) (long m)]) (get out r {}))]))}))

(defn accumulate-raster
  "Sequence of source forms accumulating reaction contributions into du."
  [net rate-laws layout]
  (let [{:keys [rs in out]} (precompute-rxn net layout)]
    (for [r rs
          :let [law (get rate-laws r)
                in-pairs  (get in r)
                out-pairs (get out r)
                [rate-form _] (rate-emit law layout in-pairs)]]
      (reaction-block rate-form in-pairs out-pairs))))

(defn accumulate-clojure
  "(fn [du u t]) that accumulates reaction contributions."
  [net rate-laws layout]
  (let [{:keys [rs in out]} (precompute-rxn net layout)
        steps (vec
               (for [r rs
                     :let [law (get rate-laws r)
                           in-pairs  (get in r)
                           out-pairs (get out r)
                           [_ rate-fn] (rate-emit law layout in-pairs)]]
                 (reaction-step-clj rate-fn in-pairs out-pairs)))]
    (fn accumulate [^doubles du ^doubles u t]
      (doseq [^clojure.lang.IFn step steps]
        (step du u t)))))

;; ============================================================================
;; ReactionDynamics — bundles a network + rate laws as RasterCompilable
;; ============================================================================

(defrecord ReactionDynamics [net rate-laws]
  cc/RasterCompilable
  (-state-layout [_] (natural-layout net))
  (-raster-body  [_ layout] (accumulate-raster  net rate-laws layout))
  (-clojure-body [_ layout] (accumulate-clojure net rate-laws layout)))

(defn reaction-dynamics
  "Bundle a reaction network with its rate-law map as a RasterCompilable."
  [net rate-laws]
  (->ReactionDynamics net rate-laws))
