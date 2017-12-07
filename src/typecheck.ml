open Syntax

exception UnknownIdentifier
exception KindMismatch
exception TypeMismatch
exception AlreadyBinded
exception BadContexts
exception IllegalApplication
exception IllegalTypeApplication
exception TypeApplicationNotSupportedSubst

type ctx = (name * info) list
type linear_ctx = (name * ty) list
type union = linear_ctx * linear_ctx

let add_left (delta1, delta2) item =
  let contains = List.mem item delta1 || List.mem item delta2 in
  if contains then raise AlreadyBinded
  else (delta1, item :: delta2)

let add_right (delta1, delta2) item =
  let contains = List.mem item delta1 || List.mem item delta2 in
  if contains then raise AlreadyBinded
  else (item :: delta1, delta2)

let rec infer_kind gamma ty =
  match ty with
  | TVar name ->
      begin
        match List.assoc_opt name gamma with
        | Some (HasKind kind) -> kind
        | _ -> raise UnknownIdentifier
      end
  | Arrow (_, kind, _) -> kind
  | Forall (_, ty) -> infer_kind gamma ty

let check_kind gamma ty kind =
  if infer_kind gamma ty = kind then ()
  else raise KindMismatch

let rec tsubst alpha ty ty' =
  match ty' with
  | TVar n -> if n = alpha then ty else (TVar n)
  | Arrow (t1, kind, t2) -> Arrow (tsubst alpha ty t1, kind, tsubst alpha ty t2)
  | Forall (kind, ty') -> Forall (kind, tsubst alpha ty ty')

let rec isubst i r iterm =
  match iterm with
  | Bound j -> if i = j then r else (Bound j)
  | Free n -> Free n
  | App (t1, t2) -> App (isubst i r t1, csubst i r t2)
  | _ -> raise TypeApplicationNotSupportedSubst
and csubst i r cterm =
  match cterm with
  | Inf t -> Inf (isubst i r t)
  | Lam (k, ty, t) -> Lam (k, ty, csubst (i + 1) r t)
  | Lambda (k, t) -> Lambda (k, csubst (i + 1) r t)

let rec infer_type i (gamma : ctx) iterm =
  match iterm with
  | Free n ->
     begin
       match List.assoc_opt n gamma with
       | Some (HasType ty) -> Some ty
       | _ -> None
     end
  | App (t1, t2) ->
     begin
       match infer_type i gamma t1 with
       | Some (Arrow (ty, kind, ty')) ->
          check_type i gamma [] t2 ty;
          Some ty'
       | _ -> raise IllegalApplication
     end
  | TApp (t, ty) ->
     begin
       match infer_type i gamma t with
       | Some (Forall (kind, ty')) ->
          let kind = infer_kind gamma ty in
          let kind' = infer_kind gamma ty' in
          if kind = kind' then
            Some (tsubst (Local i) ty ty')
          else raise KindMismatch
       | _ -> raise IllegalTypeApplication
     end
  | _ -> None

and check_type i (gamma : ctx) (delta : linear_ctx) cterm ty =
  match cterm, ty with
  | Inf t, ty ->
     begin
       match infer_type 0 gamma t with
       | Some inferred when inferred = ty -> ()
       | _ -> raise TypeMismatch
     end
  | Lam (kind, ty, term), Arrow (t, kind', t') ->
     if kind = kind' then
       let (g, d) = match kind with
           Star when delta = [] -> (((Local i, HasType ty) :: gamma), delta)
         | Pop -> (gamma, ((Local i, ty) :: delta))
         | _ -> raise BadContexts
       in
       check_type (i + 1) g d (csubst 0 (Free (Local i)) term) t'
     else raise KindMismatch
  | Lambda (kind, term), Forall (kind', ty) ->
     let alpha = (Local i, HasType ty) in
     if kind = kind' && not(List.mem alpha gamma) then
         check_type (i + 1) (alpha :: gamma) delta term ty
     else raise KindMismatch
  | _, _ -> raise TypeMismatch

