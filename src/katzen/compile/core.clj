(ns katzen.compile.core
  "Framework for compiling categorical objects (Petri nets, UWD-composed
   dynamical systems, future schemas) into raster-typed ODE right-hand
   sides.

   Follows the architecture every Catlab-family package converged on:
   walk the ACSet (no separate IR), emit source forms, hand to the
   compiler. Our equivalent of Julia's `mk_function` is raster's
   `raster.core/ftm` macro — a typed reify that the regular Clojure
   compiler emits straight-line bytecode for.

   The protocol's three responsibilities:

   1. `state-layout` — return a `StateLayout` describing the state
      vector this concept emits into. The layout's `:size` is the
      length of `u`/`du`; the layout's `:index-of` is a map from the
      concept's *natural* state labels (e.g. species ids for a Petri
      net, port ids for a UWD vertex) to integer slots in u.

   2. `raster-body` — given a (possibly enclosing) layout, return a
      *sequence of source forms* that ACCUMULATE this concept's
      contributions to `du`. The body must NOT zero `du` — the driver
      does that at the top of the emitted ftm. This separation is what
      lets composite forms concatenate per-box bodies cleanly; when two
      boxes attached to the same UWD junction both write to the same
      global state slot, summing-into-du is automatically correct.

   3. `clojure-body` — same shape but as a vanilla Clojure
      `(fn [du u t])` that accumulates. Used for debugging, autograd,
      and one-shot evaluation where the JIT warmup of `raster-body`
      isn't worth it (a pattern Catlab calls out explicitly).

   Compilation drivers:

   - `compile-rhs`        — eval the raster body into a typed
                            `IFn__doubles_doubles_double` consumable by
                            `raster.ode/solve`.
   - `compile-clojure-rhs` — same but returns the vanilla Clojure fn.

   To make a new categorical concept compilable, implement the protocol.
   No changes to `katzen.compile.core` are needed for new concepts.")

;; ============================================================================
;; State layout
;; ============================================================================

(defrecord StateLayout [size index-of])

(defn state-layout
  "Build a StateLayout for an ordered seq of natural state labels.
   `(state-layout-of [:a :b :c])` → {:size 3 :index-of {:a 0 :b 1 :c 2}}."
  ([labels]
   (->StateLayout (count labels) (zipmap labels (range))))
  ([size index-of]
   (->StateLayout size index-of)))

(defn slot
  "Look up the global state-vector index for `label` under `layout`. Throws
   on a missing label; this is the universal way a `raster-body` translates
   its concept's natural labels into global indices."
  [layout label]
  (or (get (:index-of layout) label)
      (throw (ex-info "Unknown state label" {:label label
                                             :known (keys (:index-of layout))}))))

;; ============================================================================
;; Protocol
;; ============================================================================

(defprotocol RasterCompilable
  "Categorical objects that compile to a raster ODE rhs.

   See the namespace docstring for the contract: bodies accumulate,
   layouts name slots, the driver handles zero-out."
  (-state-layout [x]
    "Return the natural StateLayout for x's state vector.")

  (-raster-body [x layout]
    "Return a sequence of source forms that accumulate du contributions
     under the given layout. The forms can use the symbols `du`, `u`,
     and `t` — these match the parameter names the driver introduces
     into the surrounding ftm. Symbols inside the forms should be
     fully qualified (e.g. raster.arrays/aset) so eval lands in a
     known environment.")

  (-clojure-body [x layout]
    "Return a vanilla (fn [du u t]) that accumulates du contributions
     under the given layout. Used as the slow-path reference and as a
     fallback when raster isn't available."))

(defn compilable? [x] (satisfies? RasterCompilable x))

;; ============================================================================
;; Public accessors (thin wrappers that allow passing an override layout)
;; ============================================================================

(defn layout-of
  "Public accessor for x's natural state layout."
  [x]
  (-state-layout x))

(defn raster-body
  "Body of source forms for x under the given layout (defaulting to x's
   own natural layout)."
  ([x]        (-raster-body x (layout-of x)))
  ([x layout] (-raster-body x layout)))

(defn clojure-body
  "Clojure (fn [du u t]) body for x under the given layout (defaulting to
   x's own natural layout)."
  ([x]        (-clojure-body x (layout-of x)))
  ([x layout] (-clojure-body x layout)))

;; ============================================================================
;; Form-builder helpers — used by both per-concept emitters and the driver
;; ============================================================================

(defn emit-zero-out
  "Source forms zeroing every du slot in `layout`."
  [layout]
  (for [i (range (:size layout))]
    `(raster.arrays/aset ~'du ~i 0.0)))

(defn ftm-form
  "Wrap a sequence of body forms in the raster typed-fn header.
   This is the standard emit shape: zero du, then accumulate."
  [layout body-forms]
  `(raster.core/ftm
    [~'du :- (~'Array ~'double) ~'u :- (~'Array ~'double) ~'t :- ~'Double]
    ~@(emit-zero-out layout)
    ~@body-forms))

;; ============================================================================
;; Drivers
;; ============================================================================

(defn compile-rhs
  "Compile x to a raster-typed RHS. Returns an
   IFn__doubles_doubles_double instance ready for `raster.ode/solve`.

   Optional `layout` lets you embed x's emission inside a larger state
   vector — useful when wrapping an external pipeline that already has
   a layout in hand."
  ([x] (compile-rhs x (layout-of x)))
  ([x layout]
   (eval (ftm-form layout (raster-body x layout)))))

(defn compile-clojure-rhs
  "Compile x to a vanilla Clojure `(fn [du u t])` that zeros du then
   accumulates contributions. This is the slow but raster-free path."
  ([x] (compile-clojure-rhs x (layout-of x)))
  ([x layout]
   (let [accumulate (clojure-body x layout)
         n          (:size layout)]
     (fn rhs [^doubles du ^doubles u t]
       (dotimes [i n] (aset du i 0.0))
       (accumulate du u t)))))
