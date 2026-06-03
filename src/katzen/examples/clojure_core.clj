(ns katzen.examples.clojure-core
  "A GAT encoding of a minimal Clojure kernel.

  This demonstrates how to encode programming language syntax and semantics
  using Generalized Algebraic Theories:

  - Theory: Abstract syntax (the forms you can write)
  - Models: Semantics (what those forms mean)

  Different models provide different interpretations:
  - StandardEval: Call-by-value evaluation
  - SymbolicClojure: AST construction
  - TypeInfer: Type inference (future)
  - Compiler: Code generation (future)"
  (:require [katzen.theory :refer [deftheory]]))

(deftheory ThClojureCore
  ;; Core types
  (type Expr)      ; Expressions (syntax)
  (type Value)     ; Runtime values
  (type Env)       ; Environment (Symbol → Value mapping)

  ;; Literal constructors
  (term lit-num
    :args [n Expr]
    :ret Expr)

  (term lit-str
    :args [s Expr]
    :ret Expr)

  (term lit-bool
    :args [b Expr]
    :ret Expr)

  (term lit-nil
    :ret Expr)

  ;; Variables
  (term var
    :args [name Expr]
    :ret Expr)

  ;; Functions (multi-arity)
  (term fn-expr
    :args [params Expr, body Expr]
    :ret Expr)

  ;; Application (multi-argument)
  (term app
    :args [fn-expr Expr, args Expr]
    :ret Expr)

  ;; Let binding (single binding for simplicity)
  (term let-expr
    :args [name Expr, val Expr, body Expr]
    :ret Expr)

  ;; Recursive let (letrec)
  (term letrec-expr
    :args [name Expr, params Expr, body Expr, in-expr Expr]
    :ret Expr)

  ;; Conditionals
  (term if-expr
    :args [test Expr, then-expr Expr, else-expr Expr]
    :ret Expr)

  ;; Sequencing
  (term do-expr
    :args [expr1 Expr, expr2 Expr]
    :ret Expr)

  ;; Primitives (represented as values)
  (term prim-add
    :ret Expr)

  (term prim-sub
    :ret Expr)

  (term prim-mul
    :ret Expr)

  (term prim-div
    :ret Expr)

  (term prim-eq
    :ret Expr)

  (term prim-lt
    :ret Expr)

  (term prim-gt
    :ret Expr)

  ;; Data structure literals
  (term lit-vec
    :args [elems Expr]
    :ret Expr)

  (term lit-map
    :args [kvs Expr]
    :ret Expr)

  ;; Data structure operations
  (term prim-nth
    :ret Expr)

  (term prim-count
    :ret Expr)

  (term prim-conj
    :ret Expr)

  (term prim-get
    :ret Expr)

  (term prim-assoc
    :ret Expr)

  ;; Environment operations (used by models)
  (term empty-env
    :ret Env)

  (term extend-env
    :args [env Env, name Expr, val Value]
    :ret Env)

  (term lookup-env
    :args [env Env, name Expr]
    :ret Value)

  ;; NO AXIOMS!
  ;; This is a free theory - models define semantics
  )
