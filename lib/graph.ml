open Core

type dual =
  { v : float
  ; d : float
  }
[@@deriving sexp]

type expr =
  | Variable of dual
  | Constant of dual
  | Sin of expr * dual
  | Exp of expr * dual
  | Add of expr * expr * dual
  | Sub of expr * expr * dual
  | Mul of expr * expr * dual
  | Div of expr * expr * dual
[@@deriving sexp]

let string_of_expr x = x |> sexp_of_expr |> Sexp.to_string

let value = function
  | Variable d -> d.v
  | Constant d -> d.v
  | Sin (_, d) -> d.v
  | Exp (_, d) -> d.v
  | Add (_, _, d) -> d.v
  | Sub (_, _, d) -> d.v
  | Mul (_, _, d) -> d.v
  | Div (_, _, d) -> d.v
;;

let deriv = function
  | Variable d -> d.d
  | Constant d -> d.d
  | Sin (_, d) -> d.d
  | Exp (_, d) -> d.d
  | Add (_, _, d) -> d.d
  | Sub (_, _, d) -> d.d
  | Mul (_, _, d) -> d.d
  | Div (_, _, d) -> d.d
;;

let variable x = Variable { v = x; d = 1.0 }
let const x = Constant { v = x; d = 0.0 }
let sin x = Sin (x, { v = Float.sin (value x); d = Float.cos (value x) })
let exp x = Exp (x, { v = Float.exp (value x); d = Float.exp (value x) })
let add x y = Add (x, y, { v = value x +. value y; d = deriv x +. deriv y })
let sub x y = Add (x, y, { v = value x -. value y; d = deriv x -. deriv y })

let mul x y =
  Mul (x, y, { v = value x *. value y; d = (deriv x *. value y) +. (deriv y *. value x) })
;;
let div x y =
  Div (x, y, { v = value x /. value y; d = (deriv x *. value y) -. (deriv y *. value x) /. (Float.int_pow (value y) 2) })
;;

let rec backward (x : expr) (f : expr) =
  match phys_equal f x with
  | true -> 1.0
  | false -> backwardh x f

and backwardh (x : expr) = function
  | Mul (l, r, _) -> (backward x l *. value r) +. (backward x r *. value l)
  | Add (l, r, _) -> backward x l +. backward x r
  | Sub (l, r, _) -> backward x l -. backward x r
  | Div (l, r, _) -> ((backward x l *. value r) -. (backward x r *. value l)) /. (Float.int_pow (value r) 2)
  | Constant _ -> 0.0
  | Variable _ -> 0.0
  | Sin (e, _) -> Float.cos (value e) *. backward x e
  | Exp (e, _) -> Float.exp (value e) *. backward x e
;;

let v_1 = variable 1.5
let v0 = variable 0.5
let v1 = div v_1 v0
let v2 = sin v1
let v3 = exp v0
let v4 = sub v1 v3
let v5 = add v2 v4
let y = mul v5 v4
let print_float f = f |> Float.to_string |> print_endline

let print_rounded_float f =
  f |> Float.round_decimal ~decimal_digits:4 |> Float.to_string |> print_endline
;;

let%expect_test "" =
  backward y y |> print_float;
  [%expect {| 1. |}]
;;

let%expect_test "" =
  backward v5 y |> print_rounded_float;
  [%expect {| 1.3513 |}]
;;

let%expect_test "" =
  let _ =
    match y with
    | Mul (v5, v4, _) ->
      value v5 |> print_rounded_float;
      value v4 |> print_rounded_float
    | _ -> failwith ""
  in
  [%expect {| 
  1.4924
  1.3513
   |}]
;;

let%expect_test "" =
  let _ =
    match y with
    | Mul (l, r, _) ->
      phys_equal l v5 |> Bool.to_string |> print_endline;
      phys_equal r v4 |> Bool.to_string |> print_endline
    | _ -> failwith ""
  in
  [%expect {| 
  true
  true 
  |}]
;;

let%expect_test "" =
  let _ =
    match v5 with
    | Add (l, r, _) ->
      phys_equal l v2 |> Bool.to_string |> print_endline;
      phys_equal r v4 |> Bool.to_string |> print_endline
    | _ -> failwith ""
  in
  [%expect {| 
  true
  true 
  |}]
;;

let%expect_test "" =
  let _ =
    match y with
    | Mul (l, r, _) ->
      backward v5 l |> print_float;
      backward v4 r |> print_float;
      backward v4 l |> print_float
    | _ -> failwith ""
  in
  [%expect {| 
  1.
  1.
  1.
  |}]
;;

let%expect_test "" =
  backward v4 y |> print_rounded_float;
  backward v2 y |> print_rounded_float;
  backward v1 y |> print_rounded_float;
  [%expect {| 
  2.8437 
  1.3513 
  1.5059 
  |}]
;;
