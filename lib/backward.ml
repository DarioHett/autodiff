(* Reference: https://github.com/Rufflewind/revad/blob/83a86f458e7d72d45253ef805675f80e3700eab0/src/tape.rs *)
open Core

type var =
  { value : float [@sexp.float]
  ; index : int [@sexp.int]
  }
[@@deriving sexp]

type 'a node = Node of 'a * 'a [@@deriving sexp]

let weights (Node (l, r) : var node) = l.value, r.value
let deps (Node (l, r) : var node) = l.index, r.index

type tape = var node list [@@deriving sexp]
type grad = Grad of float array [@@deriving sexp]

let string_of_var v = v |> sexp_of_var |> Sexp.to_string
let string_of_tape (t : tape) = t |> sexp_of_tape |> Sexp.to_string
let init () : tape = []

let push0 (tape : tape) =
  let index = List.length tape in
  let node = Node ({ value = 0.0; index }, { value = 0.0; index }) in
  let new_tape = List.append tape [ node ] in
  new_tape, index
;;

let push1 (tape : tape) (v : var) =
  let index = List.length tape in
  let node = Node ({ value = v.value; index = v.index }, { value = 0.0; index }) in
  let new_tape = List.append tape [ node ] in
  new_tape, index
;;

let push2 (tape : tape) (v : var) (w : var) =
  let index = List.length tape in
  let node =
    Node ({ value = v.value; index = v.index }, { value = w.value; index = w.index })
  in
  let new_tape = List.append tape [ node ] in
  new_tape, index
;;

let var (tape : tape) value : tape * var =
  let new_tape, index = push0 tape in
  let var = { value; index } in
  new_tape, var
;;

let sin (t : tape) (v : var) =
  let new_tape, index = push1 t { value = Float.cos v.value; index = v.index } in
  let var = { value = Float.sin v.value; index } in
  new_tape, var
;;

let add (t : tape) (v : var) (w : var) =
  let new_tape, index =
    push2 t { value = 1.0; index = v.index } { value = 1.0; index = w.index }
  in
  let var = { value = v.value +. w.value; index } in
  new_tape, var
;;

let mul (t : tape) (v : var) (w : var) =
  let new_tape, index =
    push2 t { value = w.value; index = v.index } { value = v.value; index = w.index }
  in
  let var = { value = v.value *. w.value; index } in
  new_tape, var
;;

let grad (t : tape) (v : var) =
  let open Core in
  let open Float in
  let len = List.length t in
  let derivs = Array.init len ~f:(fun _ -> 0.0) in
  Array.set derivs v.index 1.0;
  List.rev t
  |> List.iteri ~f:(fun i node ->
    let deriv = Array.get derivs Int.(len - i - 1) in
    let wl, wr = weights node in
    let dep_l, dep_r = deps node in
    Array.set derivs dep_l (Array.get derivs dep_l + (deriv * wl));
    Array.set derivs dep_r (Array.get derivs dep_r + (deriv * wr)));
  Grad derivs
;;

let wrt (Grad g : grad) (v : var) = Array.get g v.index
let string_of_grad g = sexp_of_grad g |> Sexp.to_string

let%expect_test "" =
  let t = init () in
  let t, x = var t 0.5 in
  let t, y = var t 4.2 in
  let t, b = sin t x in
  let t, a = mul t x y in
  let t, z = add t a b in
  let gradients = grad t z in
  let open Float in
  abs (z.value - 2.579425538604203) <= 1e-15 |> Bool.to_string |> print_endline;
  abs (wrt gradients x) - (y.value + cos x.value)
  <= 1e-15
  |> Bool.to_string
  |> print_endline;
  abs (wrt gradients y - x.value) <= 1e-15 |> Bool.to_string |> print_endline;
  gradients |> string_of_grad |> print_endline;
  [%expect {|
 true
 true
 true
 (Grad(5.0775825618903729 0.5 1 1 1))
 |}]
;;
