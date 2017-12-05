module Either = Base.Either

type kind =
  Star
| Pop

type ty = 
  TVar of int
| Arrow of ty * kind * ty
| Forall of int * kind * ty

type expr =
  EVar of int
| Lam of kind * int * ty * expr
| App of expr * expr
| Lambda of int * kind * value
and value =
  VLam of kind * int * ty * expr
| VLambda of int * kind * value

type ctx = (int * (ty, kind) Either.t) list
type linear_ctx = (int * ty) list
