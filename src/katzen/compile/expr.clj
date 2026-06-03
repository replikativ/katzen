(ns katzen.compile.expr
  "A tiny arithmetic-expression compiler shared by the symbolic dynamics
   constructors (`katzen.ode/vector-field`, `katzen.dwd.dynamics/
   vector-machine`).

   An expression is ordinary Clojure-shaped arithmetic. Leaves are
   resolved by a caller-supplied `leaf` fn `(sym → source-form | nil)` —
   this is where a state label becomes a state-vector read, an input
   label becomes an input read, and a parameter becomes a literal; the
   different call sites (raster vs Clojure, dynamics vs readout) differ
   only in their `leaf`. Operator forms `(+ - * /)` map to the typed
   `raster.numeric` ops (folded to binary) for the raster flavour, or
   pass through unchanged for the Clojure flavour; any other head (e.g.
   `Math/pow`) passes through in both."
  (:refer-clojure :exclude [compile]))

(def ^:private raster-binops
  '{+ raster.numeric/+, - raster.numeric/-, * raster.numeric/*, / raster.numeric//})

(defn- fold-binary
  "Left-fold an n-ary op to nested binary calls (raster.numeric ops are binary)."
  [op args]
  (reduce (fn [a b] (list op a b)) args))

(defn raster-expr
  "Compile `expr` to raster source. `leaf` maps a leaf symbol to its
   source form (or nil if unknown → error)."
  [expr leaf]
  (cond
    (number? expr) (double expr)
    (symbol? expr) (or (leaf expr)
                       (throw (ex-info "Unknown symbol in expression" {:sym expr})))
    (seq? expr)
    (let [[op & args] expr
          cargs       (mapv #(raster-expr % leaf) args)]
      (if-let [rop (get raster-binops op)]
        (cond
          (and (= op '-) (= 1 (count cargs))) (list rop 0.0 (first cargs))
          (and (= op '/) (= 1 (count cargs))) (list rop 1.0 (first cargs))
          :else                               (fold-binary rop cargs))
        (cons op cargs)))
    :else (throw (ex-info "Unsupported expression" {:expr expr}))))

(defn clj-expr
  "Compile `expr` to vanilla Clojure source (clojure.core ops are
   variadic, so no folding). `leaf` as in `raster-expr`."
  [expr leaf]
  (cond
    (number? expr) (double expr)
    (symbol? expr) (or (leaf expr)
                       (throw (ex-info "Unknown symbol in expression" {:sym expr})))
    (seq? expr) (cons (first expr) (map #(clj-expr % leaf) (rest expr)))
    :else (throw (ex-info "Unsupported expression" {:expr expr}))))
