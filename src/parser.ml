open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Syntax

module Parser : sig
  val parse : string -> term
end = struct
  let rec kind_of_sexp = function
  | Atom("*") -> Star
  | Atom("o") -> Pop
  | _ -> failwith "Unknown kind"

  let rec type_of_sexp = function
  | Atom(k) -> TVar (Local (int_of_string k))
  | List(sexps) ->
    match sexps with
    | [Atom("->"); kind; ty; ty'] ->
      let k = kind_of_sexp kind in
      let t = type_of_sexp ty in
      let t' = type_of_sexp ty' in
      Arrow(k, t, t')
    | [Atom("forall"); kind; ty] ->
      let k = kind_of_sexp kind in
      let t = type_of_sexp ty in
      Forall(k, t)
    | _ -> failwith "Parse error"

  let rec term_of_sexp = function
  | Atom(k) -> Var (Local (int_of_string k))
  | List(sexps) ->
    match sexps with
    | [Atom("lam"); kind; typ; exp] ->
      let k = kind_of_sexp kind in
      let t = type_of_sexp typ in
      let e = term_of_sexp exp in
      Lam(k, t, e)
    | [Atom("tlam"); kind; exp] ->
      let k = kind_of_sexp kind in
      let e = term_of_sexp exp in
      TLam(k, e)
    | [a; b] ->
      let t = term_of_sexp a in
      let t' = term_of_sexp b in
      App(t, t')
    | [Atom("t"); a; b] ->
      let t = term_of_sexp a in
      let t' = type_of_sexp b in
      TApp(t, t')
    | _ -> failwith "Parse error"

  and parse s = term_of_sexp (Sexp.of_string s)
end
