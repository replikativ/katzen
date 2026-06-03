(ns katzen.acset.morphism
  "ACSet morphisms — the morphisms in the category C-Set.

   An ACSet morphism φ: X → Y between two instances on the same schema C
   consists of, for every object O of C, a function φ_O : X(O) → Y(O),
   such that for every hom f: A → B in C the *naturality square* commutes:

     φ_B(f_X(p)) = f_Y(φ_A(p))    for every src-part p of A.

   Naturality is what makes φ a *natural transformation* between the
   functors X, Y : C → Set. It's also the structural property the
   homomorphism backtracker (`katzen.acset.homomorphism`) enumerates;
   that namespace returns a stream of `components` maps that this
   namespace lifts into typed `ACSetMorphism` values plus a check.

   For schemas with declared axioms, a morphism between two
   axiom-satisfying instances *automatically* preserves the axioms:
   they hold pointwise in both endpoints, and naturality makes the
   diagrams commute. The corresponding runtime check is therefore
   already covered by `katzen.acset.check/check-axioms!` applied to
   src and tgt independently.

   What this namespace ships:

     - `acset-morphism`      structural constructor with validation
     - `components`-builder  for the common 'parts-1-to-1' identity case
     - `natural?`            predicate
     - `naturality-failures` diagnostic for which squares fail
     - `compose`             composition of morphisms (associative)
     - `identity-morphism`   id_X for any X"
  (:require [katzen.acset :as a]))

;; ============================================================================
;; ACSetMorphism record
;; ============================================================================

(defrecord ACSetMorphism [src tgt components]
  Object
  (toString [_]
    (str "ACSetMorphism(|" (count (:objects (a/schema src))) " obs|, src→tgt)")))

(defn acset-morphism?
  [x] (instance? ACSetMorphism x))

;; ============================================================================
;; Construction with validation
;; ============================================================================

(defn- validate!
  "Throws on the first inconsistency: components must cover every src-part
   of every object, and image values must be actual tgt-parts."
  [src tgt components]
  (when-not (= (a/schema src) (a/schema tgt))
    (throw (ex-info "ACSet morphism: src and tgt must share a schema"
                    {:src-schema (:name (a/schema src))
                     :tgt-schema (:name (a/schema tgt))})))
  (let [schema (a/schema src)]
    (doseq [O (:objects schema)]
      (let [comp     (get components O)
            src-parts (set (a/parts src O))
            tgt-parts (set (a/parts tgt O))]
        (when-not (map? comp)
          (throw (ex-info "ACSet morphism: component missing for object"
                          {:object O})))
        (doseq [p src-parts]
          (when-not (contains? comp p)
            (throw (ex-info "ACSet morphism: component undefined on a src part"
                            {:object O :part p}))))
        (doseq [[p q] comp]
          (when-not (contains? src-parts p)
            (throw (ex-info "ACSet morphism: component domain has a non-src part"
                            {:object O :part p})))
          (when-not (contains? tgt-parts q)
            (throw (ex-info "ACSet morphism: component image is not a tgt part"
                            {:object O :value q}))))))))

(defn acset-morphism
  "Build an `ACSetMorphism` from `src` to `tgt` with the given component
   map. `components` is `{object-name → {src-part → tgt-part}}`.
   Validates that every src-part is mapped and every image is a tgt-part.
   Does NOT verify naturality — use `natural?` for that."
  [src tgt components]
  (validate! src tgt components)
  (->ACSetMorphism src tgt components))

(defn from-flat-components
  "Construct an ACSetMorphism from a flat `{[ob part] → tgt-part}` map —
   the shape returned by `katzen.acset.homomorphism/homomorphisms` and
   the datalog variant. Reshapes into the nested `{ob → {p → q}}` form
   the constructor expects."
  [src tgt flat-components]
  (acset-morphism
   src tgt
   (reduce (fn [acc [[ob p] q]] (assoc-in acc [ob p] q))
           {}
           flat-components)))

(defn identity-morphism
  "id_X : X → X. Component for each object is the identity map on its
   parts."
  [acset]
  (let [schema (a/schema acset)
        components (into {} (for [O (:objects schema)]
                              [O (into {} (for [p (a/parts acset O)]
                                            [p p]))]))]
    (->ACSetMorphism acset acset components)))

;; ============================================================================
;; Naturality
;; ============================================================================

(defn naturality-failures
  "Return a vector of {:hom :src-part :phi-A :tgt-of-phi :phi-B-of-src}
   maps, one per failing naturality square. Empty vector = morphism
   is natural.

   For a hom f: A → B and a src-part p of A:
     phi-A         := φ_A(p)               — component on A applied to p
     tgt-of-phi    := f_Y(φ_A(p))          — f in tgt applied to phi-A
     phi-B-of-src  := φ_B(f_X(p))          — φ on B applied to f_X(p)
   The square commutes iff `tgt-of-phi = phi-B-of-src`.

   When `f_X(p)` is unset (a partial morphism), that binding is skipped
   — matches our other partial-morphism conventions."
  [{:keys [src tgt components] :as _morph}]
  (let [schema (a/schema src)]
    (vec
     (for [{f :name f-dom :dom f-codom :codom} (:homs schema)
           p     (a/parts src f-dom)
           :let  [src-image    (a/subpart src f p)
                  phi-A        (get-in components [f-dom p])
                  tgt-of-phi   (a/subpart tgt f phi-A)
                  phi-B-of-src (when (some? src-image)
                                 (get-in components [f-codom src-image]))]
           :when (and (some? src-image)
                      (not= tgt-of-phi phi-B-of-src))]
       {:hom f
        :src-part p
        :phi-A phi-A
        :tgt-of-phi tgt-of-phi
        :phi-B-of-src phi-B-of-src}))))

(defn natural?
  "Predicate: does the morphism's naturality square commute for every
   hom and every src-part?"
  [morph]
  (empty? (naturality-failures morph)))

(defn check-natural!
  "Strict variant: returns `morph` on success, throws on the first
   naturality violation."
  [morph]
  (let [fails (naturality-failures morph)]
    (when (seq fails)
      (throw (ex-info (str "ACSet morphism is not natural: " (count fails)
                           " square(s) fail")
                      {:first-failure (first fails)
                       :failure-count (count fails)}))))
  morph)

;; ============================================================================
;; Composition
;; ============================================================================

(defn compose
  "Composition of two ACSet morphisms. DIAGRAMMATIC order, the shared
   katzen convention (see `katzen.cat/compose`): the FIRST argument is
   applied first. `(compose φ ψ)` with φ: X → Y and ψ: Y → Z returns
   X → Z acting as `ψ(φ(p))` (classical `ψ ∘ φ`); requires
   `(:tgt φ) = (:src ψ)`."
  [phi psi]
  (when-not (identical? (:tgt phi) (:src psi))
    (throw (ex-info "ACSet morphism compose: tgt(φ) must equal src(ψ)"
                    {:tgt-phi (a/schema (:tgt phi))
                     :src-psi (a/schema (:src psi))})))
  (let [schema (a/schema (:src phi))
        composed-components
        (into {} (for [O (:objects schema)]
                   [O (into {} (for [[p q] (get (:components phi) O)]
                                 [p (get-in psi [:components O q])]))]))]
    (->ACSetMorphism (:src phi) (:tgt psi) composed-components)))
