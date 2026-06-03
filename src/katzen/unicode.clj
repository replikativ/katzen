(ns katzen.unicode
  "Unicode operator aliases for theory definitions.

  This namespace provides bidirectional mappings between ASCII names
  and unicode operators, following GATlab.jl conventions.

  Usage:
    - Parser accepts both forms: 'compose or '⋅
    - Internally normalizes to ASCII: 'compose
    - Pretty printer can render with unicode: ⋅

  This provides maximum flexibility while maintaining tool compatibility.")

;;; ============================================================================
;;; Alias Mappings
;;; ============================================================================

(def unicode-aliases
  "Map from canonical ASCII names to unicode operators.

  Based on GATlab.jl and Catlab.jl conventions:
  - ⋅ (center dot) for composition, not ∘
  - → for Hom/arrows
  - ⊗ for tensor product (multiplicative monoidal)
  - ⊕ for coproduct (additive monoidal)
  - Greek letters for structural morphisms"
  {'compose '⋅          ; U+22C5 - composition (not ∘!)
   'mul '⋅              ; Multiplication (alias for compose)
   'otimes '⊗           ; U+2297 - tensor product
   'oplus '⊕            ; U+2295 - coproduct/biproduct
   'Hom '→              ; U+2192 - arrow type
   'munit 'I            ; Monoidal unit (capital I)
   'mzero '0            ; Monoidal zero (for coproducts)
   'associator 'α       ; U+03B1 - associator
   'left-unitor 'λ      ; U+03BB - left unitor
   'right-unitor 'ρ     ; U+03C1 - right unitor
   'braid 'σ            ; U+03C3 - symmetry/braiding
   'unit 'e             ; Monoid unit
   'inv 'i              ; Inverse (abstract)
   'zero 'zero          ; Additive identity (kept as ASCII)
   'one 'one            ; Multiplicative identity (kept as ASCII)
   'plus '+             ; Addition
   'times '*            ; Multiplication (concrete)
   'minus '-            ; Subtraction
   'id 'id              ; Identity (kept as ASCII - short enough)
   'dom 'dom            ; Domain (kept as ASCII)
   'codom 'codom})      ; Codomain (kept as ASCII)

(def ascii-aliases
  "Reverse map from unicode operators to canonical ASCII names.

  Used for normalization: parser sees ⋅, stores 'compose internally.

  Note: When multiple ASCII names map to the same unicode symbol,
  we pick the most common/canonical one for the reverse mapping."
  {'⋅ 'compose          ; Primary: composition (mul is alias)
   '⊗ 'otimes
   '⊕ 'oplus
   '→ 'Hom
   'I 'munit
   '0 'mzero
   'α 'associator
   'λ 'left-unitor
   'ρ 'right-unitor
   'σ 'braid
   'e 'unit
   'i 'inv
   'zero 'zero
   'one 'one
   '+ 'plus
   '* 'times
   '- 'minus
   'id 'id
   'dom 'dom
   'codom 'codom})

;;; ============================================================================
;;; Normalization Functions
;;; ============================================================================

(defn normalize-name
  "Normalize a symbol to its canonical ASCII form.

  Examples:
    (normalize-name 'compose) => compose
    (normalize-name '⋅) => compose
    (normalize-name 'unknown) => unknown

  This ensures internal representation uses ASCII for:
  - Stable serialization
  - Tool compatibility (grep, search)
  - Consistent comparisons"
  [sym]
  (or (get ascii-aliases sym) sym))

(defn unicode-name
  "Get the unicode form of a symbol if available.

  Examples:
    (unicode-name 'compose) => ⋅
    (unicode-name 'otimes) => ⊗
    (unicode-name 'unknown) => unknown

  Used by pretty printer to render theories with unicode."
  [sym]
  (or (get unicode-aliases sym) sym))

(defn has-unicode-alias?
  "Check if a symbol has a unicode alias."
  [sym]
  (contains? unicode-aliases sym))

(defn has-ascii-alias?
  "Check if a unicode symbol has an ASCII alias."
  [sym]
  (contains? ascii-aliases sym))

;;; ============================================================================
;;; Pretty Printing Helpers
;;; ============================================================================

(defn render-with-unicode
  "Render a symbol with its unicode form if available.

  Options:
    :show-ascii? - Show ASCII in parentheses: ⋅ (compose)
    :ascii-only? - Show ASCII only: compose"
  [sym & {:keys [show-ascii? ascii-only?] :or {show-ascii? false ascii-only? false}}]
  (let [uni (unicode-name sym)
        ascii (normalize-name sym)]
    (cond
      ascii-only?
      (str ascii)

      (and show-ascii? (not= uni ascii))
      (str uni " (" ascii ")")

      :else
      (str uni))))

;;; ============================================================================
;;; Operator Categories
;;; ============================================================================

(def categorical-operators
  "Unicode operators for category theory."
  #{'⋅ '→ 'α 'λ 'ρ 'σ})

(def monoidal-operators
  "Unicode operators for monoidal structures."
  #{'⊗ '⊕ 'I '0})

(def algebraic-operators
  "Unicode operators for algebra."
  #{'⋅ 'e 'i '+ '* '-})

(defn operator-category
  "Get the category of a unicode operator."
  [sym]
  (cond
    (categorical-operators sym) :category
    (monoidal-operators sym) :monoidal
    (algebraic-operators sym) :algebra
    :else nil))

;;; ============================================================================
;;; Input Method Documentation
;;; ============================================================================

(def input-methods
  "Documentation for typing unicode symbols.

  Map from symbol to input methods in different editors."
  {'⋅ {:emacs "\\cdot" :vim ".M" :vscode "\\cdot" :name "center dot"}
   '→ {:emacs "\\to" :vim "->" :vscode "\\to" :name "rightarrow"}
   '⊗ {:emacs "\\otimes" :vim "*X" :vscode "\\otimes" :name "otimes"}
   '⊕ {:emacs "\\oplus" :vim "+Z" :vscode "\\oplus" :name "oplus"}
   'α {:emacs "\\alpha" :vim "a*" :vscode "\\alpha" :name "alpha"}
   'λ {:emacs "\\lambda" :vim "l*" :vscode "\\lambda" :name "lambda"}
   'ρ {:emacs "\\rho" :vim "r*" :vscode "\\rho" :name "rho"}
   'σ {:emacs "\\sigma" :vim "s*" :vscode "\\sigma" :name "sigma"}})

(defn input-help
  "Get input method help for a unicode symbol."
  [sym]
  (when-let [methods (get input-methods sym)]
    (str "Type '" sym " using:\n"
         "  Emacs (TeX): " (:emacs methods) "\n"
         "  Vim (digraph): Ctrl-K " (:vim methods) "\n"
         "  VS Code: " (:vscode methods) "\n"
         "  Name: " (:name methods))))

;;; ============================================================================
;;; Validation
;;; ============================================================================

(defn valid-operator?
  "Check if a symbol is a valid operator (ASCII or unicode)."
  [sym]
  (or (contains? unicode-aliases sym)
      (contains? ascii-aliases sym)))

;;; ============================================================================
;;; Quick Reference
;;; ============================================================================

(defn list-aliases
  "List all unicode aliases with their ASCII equivalents."
  []
  (sort-by first
           (map (fn [[ascii uni]]
                  {:ascii ascii
                   :unicode uni
                   :category (operator-category uni)})
                unicode-aliases)))

(defn print-alias-table
  "Print a formatted table of unicode aliases."
  []
  (println "Unicode Aliases (GATlab.jl conventions):")
  (println "")
  (println "Category Theory:")
  (doseq [{:keys [ascii unicode]} (filter #(= :category (:category %)) (list-aliases))]
    (println (format "  %s → %s" unicode ascii)))
  (println "")
  (println "Monoidal Structures:")
  (doseq [{:keys [ascii unicode]} (filter #(= :monoidal (:category %)) (list-aliases))]
    (println (format "  %s → %s" unicode ascii)))
  (println "")
  (println "Algebra:")
  (doseq [{:keys [ascii unicode]} (filter #(= :algebra (:category %)) (list-aliases))]
    (println (format "  %s → %s" unicode ascii))))
