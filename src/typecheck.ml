open Syntax

exception UnknownTypeIdentifier of string
exception UnknownIdentifier of string
exception AlreadyBinded
exception TypeError of string
exception IllegalApplication
exception IllegalTypeApplication

type ctx = (name * info) list
type linear_ctx = (name * ty) list
type union = linear_ctx * linear_ctx
type context = ctx * linear_ctx

let add_left (delta1, delta2) item =
  let contains = List.mem item delta1 || List.mem item delta2 in
  if contains then raise AlreadyBinded
  else (delta1, item :: delta2)

let add_right (delta1, delta2) item =
  let contains = List.mem item delta1 || List.mem item delta2 in
  if contains then raise AlreadyBinded
  else (item :: delta1, delta2)

let rec infer_kind i gamma ty =
  match ty with
  | TVar name ->
      begin
        match List.assoc_opt name gamma with
        | Some (HasKind kind) -> kind
        | _ -> raise (UnknownTypeIdentifier ([%derive.show: name] name))
      end
  | Arrow (kind, t1, t2) ->
     let _ = infer_kind i gamma t1 in
     let _ = infer_kind i gamma t2 in
     kind
  | Forall (k, ty) -> infer_kind (i + 1) ((Local i, HasKind k) :: gamma) ty

let check_kind i gamma ty kind =
  if infer_kind i gamma ty = kind then ()
  else raise (TypeError "Kinds mismatch")

let rec tsubst alpha ty ty' =
  match ty' with
  | TVar n -> if n = alpha then ty else (TVar n)
  | Arrow (kind, t1, t2) -> Arrow (kind, tsubst alpha ty t1, tsubst alpha ty t2)
  | Forall (kind, ty') -> Forall (kind, tsubst alpha ty ty')

let rec tysubst i ty term =
  match term with
  | App (t1, t2) -> App (tysubst i ty t1, tysubst i ty t2)
  | Lam (k, ty', t) -> Lam (k, tsubst (Local i) ty ty', tysubst (i + 1) ty t)
  | TLam (k, t) -> TLam (k, tysubst (i + 1) ty t)
  | TApp (e, t) -> TApp (tysubst i ty e, tsubst (Local i) ty t)
  | t -> t

let rec subst i r term =
  match term with
  | Var (Local j) -> if i = j then r else Var (Local j)
  | App (t1, t2) -> App (subst i r t1, subst i r t2)
  | Lam (k, ty, t) -> Lam (k, ty, subst (i + 1) r t)
  | TLam (k, t) -> TLam (k, subst (i + 1) r t)

let rec infer_type i (gamma, delta) term =
  let context = (gamma, delta) in
  match term with
  | Var n ->
     begin
       match List.assoc_opt n gamma with
       | Some (HasType ty) ->
          (* T-UVar *)
          if delta = [] then (ty, context)
          else raise (TypeError "Linear context is non-empty with unrestricted variable")
       | _ ->
          begin
            match List.assoc_opt n delta with
            | Some ty ->
               (* T-LVar *)
               let newdelta = List.remove_assoc n delta in
               (ty, (gamma, newdelta))
            | _ -> raise (UnknownIdentifier ([%derive.show: name] n))
          end
     end
  | App (e1, e2) ->
     let (ty, (g, d)) = infer_type i context e1 in
     let (ty', (g', d')) = infer_type i (g, d) e2 in
     if g' = [] then
       begin
         match ty with
         | Arrow(kind, t1, t2) when t1 == ty' -> (t2, (g', d'))
         | _ -> raise IllegalApplication
       end
     else raise (TypeError "Unused linear variables in the argument")
  | Lam (kind, ty, term) ->
     if delta = [] || kind = Pop then
       let ty_kind = infer_kind (i + 1) gamma ty in
       match List.assoc_opt (Local i) gamma, List.assoc_opt (Local i) delta with
       | None, None ->
          let (g, d) = match ty_kind with
              Star -> (((Local i, HasType ty) :: gamma), delta)
            | Pop -> (gamma, ((Local i, ty) :: delta))
          in
          let (ty', _) = infer_type (i + 1) (g, d) (subst 0 (Var (Local i)) term) in
          (Arrow (kind, ty, ty'), context)
       | Some _, _ -> raise (TypeError "Term is already in unrestricted context")
       | _, Some _ -> raise (TypeError "Term is already in linear context")
     else raise (TypeError "Linear context is non-empty with unrestricted lambda")
  | TApp (e, ty) ->
     let kind = infer_kind 0 gamma ty in
     let (ety, newcontext) = infer_type i context e in
     begin
       match ety with
       | Forall (kind', ty') ->
          if kind = kind' then (tsubst (Local i) ty ty', newcontext)
          else raise (TypeError "Kinds mismatch")
       | _ -> raise (TypeError "Type application has a bad type")
     end
  | TLam (kind, term) ->
     match List.assoc_opt (Local i) gamma with
     | Some (HasKind kind') -> raise (TypeError "Variable already has kind")
     | _ ->
        begin
          let newcontext = (((Local i), HasKind kind) :: gamma, delta) in
          let (ty, (g, d)) = infer_type (i + 1) newcontext (tysubst 0 (TVar (Local i)) term) in
          (Forall (kind, ty), context)
        end
     
and check_type i context term ty =
  match infer_type i context term with
  | (ty', _) when ty' = ty -> ()
  | _ -> raise (TypeError "Types mismatch")

let infer = infer_type 0 ([], [])
let check = check_type 0 ([], [])
