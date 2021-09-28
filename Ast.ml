open! Core

module Var = struct
  let lock = ref 0
  type t = 
    | Var of string
    | Fre of int
  let create s = Var s
  let free () = 
    let v = Fre !lock in
    let () = lock := !lock + 1 in
    v
  let eq v1 v2 =
    match (v1, v2) with
    | (Var x, Var y) -> String.equal x y
    | (Fre i, Fre j) -> Int.equal i j
    | _ -> false
  let to_str = function
    | Var s -> s
    | Fre i -> Int.to_string i
end

module Expr = struct
  type t = 
  | Var of Var.t
  | Lam of Var.t * t
  | App of t * t
  let rec to_str = function
    | Var v -> Var.to_str v
    | Lam (v, e) -> Printf.sprintf "λ%s.(%s)" (Var.to_str v) (to_str e)
    | App (e, e') -> Printf.sprintf "(%s %s)" (to_str e) (to_str e')
end

module Top = struct
  type t = Top of Expr.t
  let create e = Top e
end

(* module Static = struct
  
end *)

module Dynamic = struct
  open! Expr

  let rec subst (e, x) e' = 
    (* Printf.printf "subst: [%s, %s] %s\n" (to_str e) (Var.to_str x) (to_str e'); *)
    match e' with
    | Var x' ->
      if Var.eq x x' then e else Var x'
    | Lam (v, el) ->
      if Var.eq x v then
        let x' = Var.free () in 
        Lam (x', subst (Var x', v) el) 
      else Lam (v, subst (e, x) el)
    | App (e1, e2) ->
      App ((subst (e, x) e1), (subst (e, x) e2))

  let rec by_name = function
    | Var _ -> None
    | Lam (_, _) -> None
    | App (e, e') -> 
      match by_name e with
      | Some e -> Some (App (e, e'))
      | None ->
        match e with
        | Var _ -> Option.map (by_name e') ~f:(fun e' -> App (e, e'))
        | Lam (x, el) -> Some (subst (e', x) el)
        | App _ -> None

  let rec by_val = function
    | Var _ -> None
    | Lam (_, _) -> None
    | App (e, e') -> 
      match by_val e with
      | Some e -> Some (App (e, e'))
      | None ->
        match by_val e' with
        | Some e' -> Some (App (e, e'))
        | None ->
          match e with
          | Var _ -> None
          | Lam (x, el) -> Some (subst (e', x) el)
          | App _ -> None
end
