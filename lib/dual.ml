open Real_intf

type ('a, 'b) dual = Dual of ('a * 'b)

module Make_dual (Real : Real_intf) = struct
  type r = Real.t
  type d = Real.t
  type t = (r, d) dual

  let constant value : t = Dual (value, Real.zero)
  let variable value : t = Dual (value, Real.one)
  let exp (Dual (_r, _d)) = Dual Real.(exp _r, mul (exp _r) _d)

  let derivative f v =
    let (Dual (_, _d)) = f v in
    _d
  ;;

  let negate (Dual (_r, _d)) = Dual (Real.neg _r, Real.neg _d)
  let add (Dual (_r1, _d1)) (Dual (_r2, _d2)) = Dual Real.(add _r1 _r2, add _d1 _d2)
  let sub (Dual (_r1, _d1)) (Dual (_r2, _d2)) = Dual Real.(sub _r1 _r2, sub _d1 _d2)

  let mul (Dual (_r1, _d1)) (Dual (_r2, _d2)) =
    Dual Real.(mul _r1 _r2, add (mul _d1 _r2) (mul _d2 _r1))
  ;;

  let log (Dual (_r, _d)) = Dual Real.(log _r, div _d _r)
  let sin (Dual (_r, _d)) = Dual Real.(sin _r, mul (cos _r) _d)
end

open Float
module DualFloat = Make_dual (Float)

let%expect_test "exp(x)'|x=0.0 == 1." =
  let open DualFloat in
  let f = exp in
  let x = variable Float.zero in
  let d' = derivative f x in
  print_endline @@ Float.to_string d';
  [%expect {| 1. |}]
;;

let%expect_test "-exp(x)'|x=0.0 == -1." =
  let open DualFloat in
  let f x = x |> exp |> negate in
  let x = variable Float.zero in
  let d' = derivative f x in
  print_endline @@ Float.to_string d';
  [%expect {| -1. |}]
;;

let%expect_test "(2*x)'|x=0.0 == 2." =
  let open DualFloat in
  let f x = mul (constant 2.0) x in
  let x = variable Float.zero in
  let d' = derivative f x in
  print_endline @@ Float.to_string d';
  [%expect {| 2. |}]
;;

let%expect_test "sin(x)'|x=0.0 == 1." =
  let open DualFloat in
  let f x = sin x in
  let x = variable Float.zero in
  let d' = derivative f x in
  print_endline @@ Float.to_string d';
  [%expect {| 1. |}]
;;

let%expect_test "log(x)'|x=2.0 == 1." =
  let open DualFloat in
  let f x = log x in
  let x = variable 2.0 in
  let d' = derivative f x in
  print_endline @@ Float.to_string d';
  [%expect {| 0.5 |}]
;;

let%expect_test "Fwd: log(x1)+x1*x2-sin(x2)|x1=2;x2=5 wrt x1 == 5.5" =
  let open DualFloat in
  let f x1 x2 =
    let v1 = log x1 in
    let v2 = mul x1 x2 in
    let v3 = sin x2 in
    let v4 = add v1 v2 in
    let v5 = sub v4 v3 in
    v5
  in
  let x1 = variable 2.0 in
  let x2 = constant 5.0 in
  let d' = derivative (fun x1 -> f x1 x2) x1 in
  print_endline @@ Float.to_string d';
  [%expect {| 5.5 |}]
;;

let%expect_test "Fwd: log(x1)+x1*x2-sin(x2)|x1=2;x2=5 wrt x2 == 5.5" =
  let open DualFloat in
  let f x1 x2 =
    let v1 = log x1 in
    let v2 = mul x1 x2 in
    let v3 = sin x2 in
    let v4 = add v1 v2 in
    let v5 = sub v4 v3 in
    v5
  in
  let x1 = constant 2.0 in
  let x2 = variable 5.0 in
  let d' = derivative (fun x2 -> f x1 x2) x2 in
  print_endline @@ Core.String.prefix (Float.to_string d') 5;
  [%expect {| 1.716 |}]
;;
