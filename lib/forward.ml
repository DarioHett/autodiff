type ops =
  | Const of float
  | Var of (string * float)
  | Sum of ops * ops
  | Mul of ops * ops

let rec recursive_forward ~f ~dx =
  match f with
  | Const _ -> Const 0.
  | Var (name, _) ->
    (match dx with
     | Var (other_name, _) ->
       (match String.equal name other_name with
        | true -> Const 1.0
        | false -> Const 0.0)
     | _ -> Const 0.0)
  | Sum (x, y) -> Sum (recursive_forward ~f:x ~dx, recursive_forward ~f:y ~dx)
  | Mul (x, y) ->
    Sum (Mul (recursive_forward ~f:x ~dx, y), Mul (x, recursive_forward ~f:y ~dx))
;;

let rec compute = function
  | Mul (x, y) -> Core.Float.( * ) (compute x) (compute y)
  | Sum (x, y) -> Core.Float.( + ) (compute x) (compute y)
  | Var (_, x) -> x
  | Const x -> x
;;

let%expect_test "df/dx for f = x == 1.0" =
  let f = Var ("x", 1.0) in
  let x =
    match recursive_forward ~f ~dx:(Var ("x", 0.0)) with
    | Const y -> y
    | _ -> failwith ""
  in
  print_endline @@ Core.Float.to_string x;
  [%expect {| 1. |}]
;;

let%expect_test "df/dy for f = x == 0.0" =
  let f = Var ("x", 1.0) in
  let x =
    match recursive_forward ~f ~dx:(Var ("y", 0.0)) with
    | Const y -> y
    | _ -> failwith ""
  in
  print_endline @@ Core.Float.to_string x;
  [%expect {| 0. |}]
;;

let%expect_test "z = x**2 + 3xy + 1" =
  let x = Var ("x", 3.0) in
  let y = Var ("y", 2.0) in
  let f x y = Sum (Sum (Mul (x, x), Mul (Const 3.0, Mul (x, y))), Const 1.0) in
  let result = compute (f x y) in
  print_endline @@ Core.Float.to_string result;
  [%expect {| 28. |}];
  let result = recursive_forward ~f:(f x y) ~dx:x |> compute in
  print_endline @@ Core.Float.to_string result;
  [%expect {| 12. |}]
;;
