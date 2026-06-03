(ns katzen.compile.lint
  "Static lint pass over synthesized raster ftm forms.

   Every `RasterCompilable` body emits `(raster.arrays/aset du i …)`
   and `(raster.arrays/aget u i)` calls indexed by integer literals
   that should be in `[0, layout-size)`. A bug in a per-concept
   emitter — wrong layout passed in, off-by-one on a stride, an
   index computed against the wrong axis — can produce out-of-bounds
   indices that only surface at integration time as an
   ArrayIndexOutOfBoundsException with no provenance.

   `lint-compilable` runs the form-building pipeline against a value's
   own layout and statically checks every aset/aget against the layout
   size. Returns `:ok` or a violations vector pointing at the offending
   sub-form.

   The lint is conservative — it only checks integer-literal indices.
   Computed indices (e.g. inside loops, which we don't currently emit)
   pass without inspection. For the current emitter shape — straight-
   line unrolled body — that's complete coverage."
  (:require [clojure.walk :as walk]
            [katzen.compile.core :as cc]))

;; ============================================================================
;; Form traversal
;; ============================================================================

(defn- array-ref?
  "Recognise an aset/aget against `du` or `u` with a literal-int index.
   Returns [:op :array :index :form] or nil."
  [f]
  (when (and (seq? f) (>= (count f) 3))
    (let [head (first f)]
      (when (#{'raster.arrays/aset 'raster.arrays/aget} head)
        (let [op (if (= 'raster.arrays/aset head) :aset :aget)
              array-sym (second f)
              idx (nth f 2)]
          (when (and (#{'du 'u} array-sym) (integer? idx))
            {:op op :array array-sym :index idx :form f}))))))

(defn- collect-array-refs
  "Walk `form` and collect every aset/aget reference for inspection."
  [form]
  (let [results (volatile! [])]
    (walk/postwalk
     (fn [f]
       (when-let [ref (array-ref? f)]
         (vswap! results conj ref))
       f)
     form)
    @results))

;; ============================================================================
;; Public API
;; ============================================================================

(defn lint-form
  "Lint a raster body or full ftm form against `layout`. Returns a vec
   of violation maps; empty vec = clean."
  [layout form]
  (let [size (:size layout)]
    (vec
     (for [{:keys [index] :as ref} (collect-array-refs form)
           :when (or (neg? index) (>= index size))]
       (assoc ref :issue :out-of-bounds :layout-size size)))))

(defn lint-compilable
  "Lint a `RasterCompilable`'s emitted body against its natural layout.
   Returns `:ok` if every aset/aget index sits in `[0, size)`, else a
   non-empty vector of violations."
  [x]
  (let [layout (cc/layout-of x)
        body   (cc/raster-body x layout)
        form   (cc/ftm-form layout body)
        viols  (lint-form layout form)]
    (if (empty? viols) :ok viols)))

(defn lint!
  "Strict variant: throws on the first violation; returns `x` otherwise."
  [x]
  (let [result (lint-compilable x)]
    (when-not (= :ok result)
      (throw (ex-info (str "Compile-output lint failed: "
                           (count result) " out-of-bounds reference(s)")
                      {:violations result})))
    x))
