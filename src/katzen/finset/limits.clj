(ns katzen.finset.limits
  "Limits in the skeletal category of finite sets: terminal, products,
   equalizers, pullbacks.

   Each `terminal` / `product` / `equalizer` / `pullback` returns a limit
   map of shape `{:type … :apex <FinSet> :legs [<FinFunction>…]}` plus
   whatever metadata the universal-property morphism needs.

   `universal` is a multimethod dispatched on `:type` that builds the
   unique morphism from any compatible cone into the apex.

   Implementations follow Catlab.jl's SkelFinSet algorithms — kept simple
   because the v1 use cases (UWDs, applied-CT schemas) work at small
   cardinalities. Indexed/hash-join variants can be added later as
   `katzen.finset.indexed` if benchmark pressure shows up."
  (:require [katzen.finset :as fs]))

(defmulti universal
  "Universal arrow into a limit from a compatible cone.

   Second argument shape depends on :type:

   - :terminal     — pass the dom FinSet (or its size); returns the unique
                     constant arrow to the singleton apex.
   - :product      — pass a vector of FinFunctions [f g …] sharing a
                     common dom X, each mapping to the corresponding
                     factor; returns X → apex.
   - :equalizer    — pass one FinFunction h whose codom is the limit's
                     common-domain set such that h equalises the parallel
                     arrows; returns h's domain → apex.
   - :pullback     — pass [h k] sharing a common dom X with f∘h = g∘k;
                     returns X → apex."
  (fn [lim _] (:type lim)))

;; ============================================================================
;; Terminal: the singleton {0}
;; ============================================================================

(defn terminal
  "Terminal object: a singleton FinSet. Every set has exactly one map to it
   (the constant 0)."
  []
  {:type :terminal
   :apex (fs/fin-set 1)
   :legs []})

(defmethod universal :terminal [_lim dom]
  (let [n (cond
            (fs/fin-set? dom)   (fs/cardinality dom)
            (integer? dom)      dom
            (fs/fin-function? dom) (fs/cardinality (fs/dom dom))
            :else (throw (ex-info "Bad universal-terminal arg" {:got dom})))]
    (fs/constant-function n 0 1)))

;; ============================================================================
;; Product (binary)
;; ============================================================================

(defn product
  "Binary product of FinSets A × B. Apex has cardinality |A|·|B|; the apex
   element k is interpreted via column-major linearization, k = a + b·|A|,
   so projection-1 returns `k mod |A|` and projection-2 returns `k div |A|`.
   This matches Catlab's `LinearIndices` convention."
  [a b]
  (let [na (fs/cardinality a)
        nb (fs/cardinality b)
        n  (* na nb)
        proj1 (fs/fin-function (vec (for [k (range n)] (mod k na))) na)
        proj2 (fs/fin-function (vec (for [k (range n)] (quot k na))) nb)]
    {:type :product
     :apex (fs/fin-set n)
     :legs [proj1 proj2]
     :factor-sizes [na nb]}))

(defmethod universal :product [lim arrows]
  (let [[f g] arrows
        [na _nb] (:factor-sizes lim)
        n (count (:vals f))]
    (when-not (= n (count (:vals g)))
      (throw (ex-info "product universal: arrows must share a domain"
                      {:dom-f n :dom-g (count (:vals g))})))
    (fs/fin-function
     (vec (for [i (range n)]
            (+ (fs/app f i) (* (fs/app g i) na))))
     (:apex lim))))

(defn product-n
  "n-ary product: fold of binary products. Returns a limit map with
   `:legs` of length n. The apex element k is the linearized
   (k0, k1, ..., k(n-1)) under column-major indexing across factors."
  [factors]
  (cond
    (empty? factors)        (terminal)
    (= 1 (count factors))   (let [a (first factors)]
                              {:type :product
                               :apex a
                               :legs [(fs/id-function a)]
                               :factor-sizes [(fs/cardinality a)]})
    :else
    (let [;; Sizes per factor.
          sizes (mapv fs/cardinality factors)
          n     (reduce * 1 sizes)
          ;; Cumulative strides per factor: stride[0] = 1, stride[i] = prod sizes[0..i-1].
          strides (vec (reductions * 1 (butlast sizes)))
          proj-i (fn [i]
                   (let [s (nth strides i)
                         m (nth sizes i)]
                     (fs/fin-function
                      (vec (for [k (range n)] (mod (quot k s) m)))
                      m)))]
      {:type :product
       :apex (fs/fin-set n)
       :legs (mapv proj-i (range (count factors)))
       :factor-sizes sizes
       :strides strides})))

;; ============================================================================
;; Equalizer of a pair f, g : A → B
;; ============================================================================

(defn equalizer
  "Equalizer of two parallel FinFunctions f, g : A → B. The apex is the
   subset {a ∈ A | f(a) = g(a)}, embedded into A by the inclusion.
   Stored sorted so the universal property can binary-search."
  [f g]
  (when-not (= (fs/dom f) (fs/dom g))
    (throw (ex-info "equalizer: parallel arrows must share a domain"
                    {:dom-f (fs/dom f) :dom-g (fs/dom g)})))
  (when-not (= (fs/cod f) (fs/cod g))
    (throw (ex-info "equalizer: parallel arrows must share a codomain"
                    {:cod-f (fs/cod f) :cod-g (fs/cod g)})))
  (let [na (fs/cardinality (fs/dom f))
        eq-vec (vec (filter #(= (fs/app f %) (fs/app g %)) (range na)))]
    {:type :equalizer
     :apex (fs/fin-set (count eq-vec))
     :legs [(fs/fin-function eq-vec na)]
     :parallel [f g]
     :inclusion-vec eq-vec}))

(defmethod universal :equalizer [lim h]
  ;; h: X → A factors through the inclusion when h(x) ∈ inclusion-vec.
  ;; The induced map sends x to the position of h(x) in inclusion-vec.
  (let [incl (:inclusion-vec lim)
        pos  (zipmap incl (range))
        n    (fs/cardinality (fs/dom h))]
    (fs/fin-function
     (vec (for [i (range n)]
            (or (pos (fs/app h i))
                (throw (ex-info "Universal property failed: h does not equalize"
                                {:i i :h-of-i (fs/app h i)})))))
     (:apex lim))))

;; ============================================================================
;; Pullback of a cospan f: A → C, g: B → C
;; ============================================================================

(defn pullback
  "Pullback of a cospan f: A → C, g: B → C. Apex = {(a, b) ∈ A × B | f(a) = g(b)}.
   Legs are the two projections back to A and B.

   Algorithm: enumerate A × B once (naive, O(|A|·|B|)); the survey notes
   indexed hash-join variants for larger problems, deferred for now."
  [f g]
  (when-not (= (fs/cod f) (fs/cod g))
    (throw (ex-info "pullback: cospan arrows must share a codomain"
                    {:cod-f (fs/cod f) :cod-g (fs/cod g)})))
  (let [na    (fs/cardinality (fs/dom f))
        nb    (fs/cardinality (fs/dom g))
        pairs (vec (for [i (range na)
                         j (range nb)
                         :when (= (fs/app f i) (fs/app g j))]
                     [i j]))
        proj1 (fs/fin-function (mapv first pairs) na)
        proj2 (fs/fin-function (mapv second pairs) nb)]
    {:type :pullback
     :apex (fs/fin-set (count pairs))
     :legs [proj1 proj2]
     :cospan [f g]
     :pairs pairs
     :pair-index (zipmap pairs (range))}))

(defmethod universal :pullback [lim arrows]
  (let [[h k] arrows
        [f g] (:cospan lim)
        idx   (:pair-index lim)
        n     (fs/cardinality (fs/dom h))]
    (when-not (= n (fs/cardinality (fs/dom k)))
      (throw (ex-info "pullback universal: arrows must share a domain"
                      {:dom-h n :dom-k (fs/cardinality (fs/dom k))})))
    ;; Sanity: f ∘ h must equal g ∘ k pointwise (commutativity of the square).
    (fs/fin-function
     (vec (for [i (range n)]
            (let [a (fs/app h i)
                  b (fs/app k i)]
              (when-not (= (fs/app f a) (fs/app g b))
                (throw (ex-info "Universal property failed: square does not commute"
                                {:i i :f-of-h-i (fs/app f a) :g-of-k-i (fs/app g b)})))
              (or (idx [a b])
                  (throw (ex-info "Pair not found in pullback apex"
                                  {:pair [a b]}))))))
     (:apex lim))))
