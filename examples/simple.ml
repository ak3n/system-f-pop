open Syntax
open Typecheck
open Format
open Parser

let _ =
  begin
    let t = Parser.parse "(tlam o (lam * 0 0))" in
    let term = TLam (Pop, Lam (Star, TVar (Local 0), Var (Local 0))) in
    assert (t = term)
  end

let _ =
  begin
    let term = Parser.parse "(tlam o (lam * 0 0))" in
    let (ty, _) = infer term in
    printf "%s\n" ([%derive.show: ty] ty);
  end

let _ =
  begin
    let term = Parser.parse "(tlam * (lam o 0 (lam * 0 0)))" in
    let (ty, _) = infer term in
    printf "%s\n" ([%derive.show: ty] ty)
  end

let _ =
  begin
    let term = Parser.parse "(tlam * (tlam o (lam o 0 (((lam o 0 (lam o 1 1)) 0) 0))))" in
    try
      let (ty, _) = infer term in
      printf "%s\n" ([%derive.show: ty] ty)
    with
      TypeError m -> printf "%s\n" m
  end
