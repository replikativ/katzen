(ns katzen.uwd.dynamics
  "Operadic composition of continuous-time dynamical systems through UWDs
   — the AlgebraicDynamics analog over our compile framework.

   A `ContinuousResourceSharer` (CRS) bundles a `RasterCompilable` value
   with the list of state-labels that are exposed at the CRS's ports.
   `oapply` takes a UWD and a per-box CRS map, computes the coequalizer
   of per-box state sets along junction identifications (a colimit in
   FinSet), and returns a composite CRS whose `raster-body` is the
   concatenation of per-box bodies — each emitted under a re-mapped
   state-layout that sends the box's natural labels to their composite
   class indices.

   Catlab's AlgebraicDynamics performs the same operation in pure
   closures (`induced_dynamics` re-walks the diagram on every RHS
   call); we instead emit a single ftm whose body has every box's math
   inlined. The composite raster-body is straight-line code — no
   per-step dispatch, no diagram traversal.

   The protocol's accumulate-not-zero contract is what makes this
   work: each box's body accumulates into `du`; when two boxes share a
   junction their corresponding states sit at the same global slot
   and the accumulations sum automatically."
  (:require [katzen.compile.core :as cc]
            [katzen.finset :as fs]
            [katzen.finset.colimits :as colim]
            [katzen.uwd :as uwd]))

;; ============================================================================
;; ContinuousResourceSharer
;; ============================================================================

(defrecord ContinuousResourceSharer
  [layout port-states raster-body-fn clojure-body-fn]
  cc/RasterCompilable
  (-state-layout [_] layout)
  (-raster-body  [_ override-layout] (raster-body-fn override-layout))
  (-clojure-body [_ override-layout] (clojure-body-fn override-layout)))

(defn from-compilable
  "Lift a `RasterCompilable` value `x` into a CRS by declaring which of
   its natural state labels are exposed at ports, in port order.

     (from-compilable petri-sir [:S :E :I :R])   ;; exposes 4 ports
     (from-compilable some-rhs   [])             ;; closed system, 0 ports

   `port-states` is a vector of labels valid for `(layout-of x)`."
  [x port-states]
  (let [layout (cc/layout-of x)
        idx-of (:index-of layout)]
    (doseq [label port-states]
      (when-not (contains? idx-of label)
        (throw (ex-info "port-state not in layout"
                        {:label label :known (keys idx-of)}))))
    (->ContinuousResourceSharer
     layout
     port-states
     (fn [override] (cc/raster-body x override))
     (fn [override] (cc/clojure-body x override)))))

;; ============================================================================
;; oapply — the operadic composition
;; ============================================================================

(defn- port-label
  "For port `p` belonging to box `b` of UWD `d`, look up which of
   `crs`'s natural state-labels the port exposes."
  [d crs b p]
  (let [port-idx (.indexOf ^java.util.List (uwd/box-ports d b) p)]
    (nth (:port-states crs) port-idx)))

(defn- consecutive-pairs
  "[a b c d] → [[a b] [b c] [c d]]. Empty for singletons or empties."
  [xs]
  (mapv vector xs (rest xs)))

(defn oapply
  "Compose `box->crs` through the UWD `d`. Returns a CRS whose state set
   is the coequalizer of (disjoint union of box state sets) along the
   junction-induced identifications, and whose dynamics are the
   superposition of per-box dynamics under the composite layout.

   The composite CRS's ports correspond to the UWD's outer-ports: each
   outer-port j becomes a composite port whose state-label is the
   composite-layout key of the class containing j's junction."
  [d box->crs]
  (let [boxes (vec (uwd/boxes d))
        per-box (mapv (fn [b] (get box->crs b)) boxes)
        per-box-layout (mapv :layout per-box)
        per-box-size   (mapv :size per-box-layout)
        offsets        (vec (reductions + 0 (butlast per-box-size)))
        offset-of      (zipmap boxes offsets)
        total          (reduce + 0 per-box-size)

        ;; Translate (box-id, state-label) → global pre-quotient index.
        global-of (fn [b label]
                    (+ (get offset-of b)
                       (cc/slot (nth per-box-layout (.indexOf ^java.util.List boxes b))
                                label)))

        ;; Build the equate-pairs for the coequalizer. For every junction
        ;; with ≥ 2 ports, consecutive ports' state-labels must be merged.
        equate-pairs
        (vec
         (for [j (uwd/junctions d)
               :let [ps (uwd/junction-ports d j)]
               :when (>= (count ps) 2)
               [p1 p2] (consecutive-pairs ps)]
           (let [b1 (uwd/port-box d p1)
                 b2 (uwd/port-box d p2)
                 l1 (port-label d (get box->crs b1) b1 p1)
                 l2 (port-label d (get box->crs b2) b2 p2)]
             [(global-of b1 l1) (global-of b2 l2)])))

        ;; Coequalizer in FinSet over the disjoint union of size `total`.
        f (fs/fin-function (mapv first  equate-pairs) total)
        g (fs/fin-function (mapv second equate-pairs) total)
        coeq (colim/coequalizer f g)
        proj (first (:legs coeq))
        n-classes (fs/cardinality (:apex coeq))
        class-of-global (fn [gi] (fs/app proj gi))

        ;; Composite layout: keys are [box-id natural-label] tuples, values
        ;; are the class index in the composite state vector.
        composite-index-of
        (into {}
              (for [b boxes
                    label (keys (:index-of (nth per-box-layout
                                                (.indexOf ^java.util.List boxes b))))]
                [[b label] (class-of-global (global-of b label))]))
        composite-layout (cc/state-layout n-classes composite-index-of)

        ;; Re-mapped layout for box b: same labels b knows, mapped to global
        ;; composite class indices.
        layout-for-box
        (memoize
         (fn [b]
           (let [bl (nth per-box-layout (.indexOf ^java.util.List boxes b))]
             (cc/state-layout
              n-classes
              (into {}
                    (for [[label _] (:index-of bl)]
                      [label (class-of-global (global-of b label))]))))))

        ;; Composite raster-body: concatenation of every box's body emitted
        ;; under the box's remapped layout. The `override-layout` arg from
        ;; an outer caller is reserved for embedding this composite into a
        ;; still-larger composite; v1 ignores it and uses composite-layout.
        raster-body-fn
        (fn [_override]
          (mapcat (fn [b] (cc/raster-body (get box->crs b) (layout-for-box b)))
                  boxes))

        clojure-body-fn
        (fn [_override]
          (let [bodies (mapv (fn [b] (cc/clojure-body (get box->crs b)
                                                     (layout-for-box b)))
                             boxes)]
            (fn accumulate [^doubles du ^doubles u t]
              (doseq [^clojure.lang.IFn body bodies]
                (body du u t)))))

        ;; Composite port-states: one per outer-port. Each outer-port is
        ;; attached to a junction; pick any inner port of that junction
        ;; and report its [box-id, label] tuple — they all hit the same
        ;; composite-index-of entry.
        composite-port-states
        (mapv
         (fn [op]
           (let [j (uwd/outer-junction d op)
                 inner-ports (uwd/junction-ports d j)]
             (when (empty? inner-ports)
               (throw (ex-info "Outer junction has no inner ports; can't expose"
                               {:outer-port op :junction j})))
             (let [p1 (first inner-ports)
                   b1 (uwd/port-box d p1)
                   l1 (port-label d (get box->crs b1) b1 p1)]
               [b1 l1])))
         (uwd/outer-ports d))]
    (->ContinuousResourceSharer
     composite-layout
     composite-port-states
     raster-body-fn
     clojure-body-fn)))
