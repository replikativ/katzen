(ns katzen.finset
  "Skeletal FinSet: the category of finite sets and functions, with the
   canonical-form choice that the object n is the set {0, 1, ..., n-1}.

   This is the FinSet that Catlab.jl calls SkelFinSet — its `FinSetInt(n)`
   represents 1..n, ours represents 0..n-1 (0-based matches Clojure
   indexing without per-call decrements). The semantics are the same.

   FinFunctions are represented as a Clojure vector of codomain elements,
   indexed by domain element: `(:vals f)` at index i is the image of i.
   Identity functions and (eager) composites are also FinFunctions —
   composition with an identity returns the other side unchanged, and
   eager composition collapses chains so `(compose (compose f g) h)` is
   one vector, not a deferred AST.

   Catlab uses more representations (dict-backed FinSetHash, lazy
   composites, indexed preimage caches, etc.) — those exist to handle
   non-skeletal sets and performance-sensitive join queries. The v1
   Clojure port targets the SkelFinSet subset, which is enough for UWDs,
   the AlgebraicPetri-style demos, and ACSet integration.")

;; ============================================================================
;; FinSet
;; ============================================================================

(defrecord FinSet [n])

(defn fin-set
  "Create the finite set {0, 1, ..., n-1}."
  [n]
  (when (neg? n)
    (throw (ex-info "FinSet size must be non-negative" {:n n})))
  (->FinSet n))

(defn fin-set? [x] (instance? FinSet x))

(defn elements
  "Sorted seq of the elements of the FinSet: (0 1 ... n-1)."
  [s]
  (range (:n s)))

(defn cardinality [s] (:n s))

;; ============================================================================
;; FinFunction
;; ============================================================================
;;
;; Internal invariant: (:vals f) is a vector of (:n (:codom f))-side ints
;; in [0, codom-n). The domain is implicit: (fin-set (count (:vals f))).

(defrecord FinFunction [vals codom])

(defn fin-function
  "Build a FinFunction. `vals` is the image vector — `(nth vals i)` is
   the image of domain element i. `codom` is the codomain size (an int)
   or a FinSet."
  [vals codom]
  (let [codom-n (if (fin-set? codom) (:n codom) codom)]
    (when-not (every? #(and (integer? %) (<= 0 %) (< % codom-n)) vals)
      (throw (ex-info "FinFunction image out of codomain range"
                      {:vals vals :codom codom-n})))
    (->FinFunction (vec vals) (fin-set codom-n))))

(defn fin-function? [x] (instance? FinFunction x))

(defn dom
  "Domain (a FinSet) of a FinFunction."
  [f]
  (fin-set (count (:vals f))))

(defn cod
  "Codomain (a FinSet) of a FinFunction."
  [f]
  (:codom f))

(defn app
  "Apply f to a domain element. Throws if out of range."
  [f x]
  (when-not (and (<= 0 x) (< x (count (:vals f))))
    (throw (ex-info "Domain element out of range"
                    {:x x :dom-size (count (:vals f))})))
  (nth (:vals f) x))

;; ============================================================================
;; Identity and composition
;; ============================================================================

(defn id-function
  "Identity function on a FinSet (or on the FinSet of size n if given
   an int). Concretely: vals = [0 1 ... n-1], codom = n."
  [s]
  (let [n (if (fin-set? s) (:n s) s)]
    (fin-function (vec (range n)) n)))

(defn identity-function?
  "Predicate: is this FinFunction the identity? Cheap structural check —
   vals[i] = i for every i and codom matches dom."
  [f]
  (and (fin-function? f)
       (= (count (:vals f)) (:n (:codom f)))
       (every? (fn [[i v]] (= i v)) (map-indexed vector (:vals f)))))

(defn compose
  "Compose f: A → B with g: B → C, returning f ⋅ g : A → C.

   CONVENTION (shared by every `compose` in katzen — finset, ACSet
   morphisms, theory morphisms; unified at `katzen.cat/compose`):
   DIAGRAMMATIC order — the FIRST argument is applied first. `(compose
   f g)` requires `codom(f) = dom(g)` and acts as `g(f(x))` (classical
   `g ∘ f`). Composition is *eager*: a fresh vector is built. Identities
   on either side short-circuit."
  [f g]
  (when-not (= (:n (cod f)) (:n (dom g)))
    (throw (ex-info "FinFunction compose: codom(f) must equal dom(g)"
                    {:codom-f (:n (cod f)) :dom-g (:n (dom g))})))
  (cond
    (identity-function? f) g
    (identity-function? g) f
    :else (fin-function (mapv #(app g %) (:vals f)) (cod g))))

;; ============================================================================
;; Constant function
;; ============================================================================

(defn constant-function
  "Build the constant function dom-size → c, where c is in 0..codom-1.
   Used by the terminal universal property."
  [dom-size c codom]
  (let [codom-n (if (fin-set? codom) (:n codom) codom)]
    (fin-function (vec (repeat dom-size c)) codom-n)))

;; ============================================================================
;; Preimage and image
;; ============================================================================

(defn preimage
  "Sorted vec of dom elements x such that f(x) = y. O(n) by default; a
   cached variant lives in katzen.finset.indexed when needed."
  [f y]
  (vec (keep-indexed (fn [i v] (when (= v y) i)) (:vals f))))

(defn image
  "Set of values in the codomain that have at least one preimage."
  [f]
  (set (:vals f)))

(defn surjective? [f] (= (image f) (set (range (:n (cod f))))))

(defn injective? [f] (= (count (set (:vals f))) (count (:vals f))))

(defn bijective? [f] (and (injective? f) (surjective? f)))

;; ============================================================================
;; Equality
;; ============================================================================

(defn fin-function=
  "FinFunctions are equal when they have the same dom, codom, and image vec."
  [f g]
  (and (fin-function? f) (fin-function? g)
       (= (:codom f) (:codom g))
       (= (:vals f) (:vals g))))
