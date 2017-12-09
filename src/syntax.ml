type name =
  Local of int
[@@deriving show]

type kind =
  Star
| Pop
[@@deriving show]

type ty = 
  TVar of name
| Arrow of kind * ty * ty
| Forall of kind * ty
[@@deriving show]

type term =
  Var of name
| App of term * term
| TApp of term * ty
| Lam of kind * ty * term
| TLam of kind * term
[@@deriving show]

type info =
  HasKind of kind
| HasType of ty
[@@deriving show]
