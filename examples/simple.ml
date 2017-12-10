open Syntax
open Typecheck
open Format


let _ =
  begin
    let term = TLam (Pop, Lam (Star, TVar (Local 0), Var (Local 0))) in
    let (ty, _) = infer term in
    printf "%s\n" ([%derive.show: ty] ty)
  end

let _ =
  begin
    let term = TLam (Star,
                     Lam (Pop,
                          TVar (Local 0),
                          Lam (Star, TVar (Local 0), Var (Local 0)))) in
    let (ty, _) = infer term in
    printf "%s\n" ([%derive.show: ty] ty)
  end

let _ =
  begin
    let term = TLam (Star,
                     TLam (Pop,
                           Lam (Pop,
                                TVar (Local 0),
                                App (App (Lam (Pop,
                                               TVar (Local 0),
                                               Lam (Pop,
                                                    TVar (Local 1),
                                                    Var (Local 1))),
                                          Var (Local 0)),
                                     Var (Local 0)))))
    in
    let (ty, _) = infer term in
    printf "%s\n" ([%derive.show: ty] ty)
  end
