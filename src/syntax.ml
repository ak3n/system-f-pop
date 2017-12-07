type name =
  Global of string
| Local of int

type kind =
  Star
| Pop

type ty = 
  TVar of name
| Arrow of ty * kind * ty
| Forall of kind * ty

type iterm =
  Bound of int
| Free of name
| App of iterm * cterm
| TApp of iterm * ty
and cterm =
  Inf of iterm
| Lam of kind * ty * cterm
| Lambda of kind * cterm

type info =
  HasKind of kind
| HasType of ty
