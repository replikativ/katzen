(ns katzen.ode
  "Vector fields as `RasterCompilable` values — the missing sibling of
   `katzen.petri/petri-dynamics`.

   A `PetriDynamics` derives its ODE right-hand side from a reaction
   network (mass-action). But many dynamical systems are *not* clean
   mass-action nets — the textbook Lotka–Volterra predator–prey model
   has independent birth/death/predation rates that don't factor as a
   single net. This namespace adds two more sources of a vector field,
   both implementing `katzen.compile.core/RasterCompilable` so they
   plug into `compile-rhs` / `compile-clojure-rhs` and compose through
   `katzen.uwd.dynamics/oapply` exactly like a Petri net does:

   - `vector-field` — a SYMBOLIC field `{state expr …}`. Expressions are
     ordinary arithmetic over the state labels and named parameters;
     they compile to BOTH a raster body (straight-line `aget`/`aset`,
     the fast path) and a Clojure body. This is what Lotka–Volterra
     *should* use — it keeps the full raster numerics and inlines under
     composition. It is strictly more than AlgebraicDynamics offers,
     which always carries opaque closures.

   - `raw-field` — an OPAQUE Clojure closure `(fn [u t] → du)` over the
     box's local state vector. This is the faithful analog of
     AlgebraicDynamics' `ContinuousResourceSharer{T}(nstates, f)`: any
     Clojure dynamics whatsoever, composed by gather→call→scatter (the
     `induced_dynamics` recipe). It has NO raster body — an opaque fn
     can't be inlined into straight-line code — so it runs only on the
     Clojure path (`compile-clojure-rhs`). AlgebraicDynamics has the
     same limitation (it re-walks closures every step).

   Both are bare `RasterCompilable`s. To use them as boxes in a UWD,
   lift with `katzen.uwd.dynamics/from-compilable` and declare ports."
  (:require [katzen.compile.core :as cc]))

;; ============================================================================
;; Expression compiler (shared by both bodies of a vector-field)
;; ============================================================================
;;
;; A field expression is ordinary Clojure-shaped arithmetic over:
;;   - state labels      → read from the state vector u at the layout slot
;;   - parameter symbols → substituted as double literals
;;   - numeric literals  → double literals
;;   - operator forms (op arg …) → `+ - * /` plus host calls (Math/pow …)
;;
;; We emit two flavours from the same expr: a raster flavour using the
;; typed `raster.numeric` / `raster.arrays` ops (matching what
;; `katzen.petri` emits), and a vanilla Clojure flavour.

(def ^:private raster-binops
  '{+ raster.numeric/+, - raster.numeric/-, * raster.numeric/*, / raster.numeric//})

(defn- fold-binary
  "Left-fold an n-ary op down to nested binary calls: (op a b c) →
   (op (op a b) c). raster.numeric ops are binary."
  [op args]
  (reduce (fn [a b] (list op a b)) args))

(defn- raster-expr
  "Compile a field expression to raster source. `idx-of` maps state
   labels to their (global) slot in the state vector named `u`."
  [expr idx-of params]
  (cond
    (contains? idx-of expr) (list 'raster.arrays/aget 'u (get idx-of expr))
    (contains? params expr) (double (get params expr))
    (number? expr)          (double expr)
    (seq? expr)
    (let [[op & args] expr
          cargs       (mapv #(raster-expr % idx-of params) args)]
      (if-let [rop (get raster-binops op)]
        (cond
          (and (= op '-) (= 1 (count cargs))) (list rop 0.0 (first cargs))
          (and (= op '/) (= 1 (count cargs))) (list rop 1.0 (first cargs))
          :else                               (fold-binary rop cargs))
        (cons op cargs)))                ; host passthrough, e.g. (Math/pow x 2)
    :else
    (throw (ex-info "Unsupported vector-field expression" {:expr expr}))))

(defn- clj-expr
  "Compile a field expression to vanilla Clojure source over the
   double-array `u` (clojure.core ops are variadic, so no folding)."
  [expr idx-of params]
  (cond
    (contains? idx-of expr) (list 'clojure.core/aget 'u (get idx-of expr))
    (contains? params expr) (double (get params expr))
    (number? expr)          (double expr)
    (seq? expr)             (cons (first expr)
                                  (map #(clj-expr % idx-of params) (rest expr)))
    :else
    (throw (ex-info "Unsupported vector-field expression" {:expr expr}))))

;; ============================================================================
;; vector-field (symbolic) — RasterCompilable with BOTH bodies
;; ============================================================================

(defn- check-field-labels [states field]
  (let [known (set states)]
    (doseq [label (keys field)]
      (when-not (known label)
        (throw (ex-info "Field key is not a declared state"
                        {:label label :states states}))))))

(defrecord VectorField [states params field]
  cc/RasterCompilable
  (-state-layout [_] (cc/state-layout states))
  (-raster-body [_ layout]
    (let [idx-of (:index-of layout)]
      (vec
       (for [[label expr] field
             :let [g (cc/slot layout label)]]
         `(raster.arrays/aset
           ~'du ~g
           (raster.numeric/+ (raster.arrays/aget ~'du ~g)
                             ~(raster-expr expr idx-of params)))))))
  (-clojure-body [_ layout]
    (let [idx-of (:index-of layout)
          form   `(fn [~(with-meta 'du {:tag 'doubles})
                       ~(with-meta 'u {:tag 'doubles})
                       ~'t]
                    ~@(for [[label expr] field
                            :let [g (cc/slot layout label)]]
                        `(clojure.core/aset
                          ~'du ~g
                          (clojure.core/+ (clojure.core/aget ~'du ~g)
                                          (double ~(clj-expr expr idx-of params))))))]
      (eval form))))

(defn vector-field
  "A symbolic vector field as a `RasterCompilable`.

     (vector-field
       {:states [r f]
        :params {alpha 1.1, beta 0.4, delta 0.1, gamma 0.4}
        :field  {r (- (* alpha r) (* beta r f))     ; dr/dt
                 f (- (* delta r f) (* gamma f))}})  ; df/dt

   `:states` is the ordered state vector (any labels — symbols read
   nicely in expressions). `:params` are substituted as constants.
   `:field` maps each state to the expression for its time-derivative;
   a missing state contributes 0. Compiles to both the fast raster body
   and a Clojure body, and composes through `oapply`."
  [{:keys [states params field]}]
  (let [states (vec states)]
    (check-field-labels states field)
    (->VectorField states (or params {}) field)))

;; ============================================================================
;; raw-field (opaque closure) — RasterCompilable, Clojure body only
;; ============================================================================

(defrecord RawField [states dynamics]
  cc/RasterCompilable
  (-state-layout [_] (cc/state-layout states))
  (-raster-body [_ _layout]
    (throw (ex-info "raw-field has no raster body — its dynamics is an opaque closure; integrate via compile-clojure-rhs"
                    {:states states})))
  (-clojure-body [_ layout]
    (let [oidx   (:index-of layout)
          gslots (mapv (fn [s] (cc/slot layout s)) states)
          n      (count gslots)]
      (fn accumulate [^doubles du ^doubles u t]
        (let [local-u  (mapv (fn [^long g] (aget u g)) gslots)
              local-du (dynamics local-u t)]
          (dotimes [i n]
            (let [g (long (nth gslots i))]
              (aset du g (+ (aget du g) (double (nth local-du i)))))))))))

(defn raw-field
  "An opaque Clojure vector field as a `RasterCompilable`.

     (raw-field
       {:states [r f]
        :dynamics (fn [[r f] _t] [(- (* 1.1 r) (* 0.4 r f))
                                  (- (* 0.1 r f) (* 0.4 f))])})

   `:dynamics` is `(fn [u t] → du)`: `u` is a Clojure vector of the
   box's local states in `:states` order, `t` is time, and the return
   is the derivative vector in the same order. This is the faithful map
   of AlgebraicDynamics' raw-function resource sharer — any dynamics at
   all, composed by gather/call/scatter. No raster body: run on the
   Clojure path (`compile-clojure-rhs`)."
  [{:keys [states dynamics]}]
  (->RawField (vec states) dynamics))
