open Core
open Autodiff
open Core_bench

let () =
  let open Graph in
  let v_1 = variable 1.5 in
  let v0 = variable 0.5 in
  let v1 = div v_1 v0 in
  let v2 = sin v1 in
  let v3 = exp v0 in
  let v4 = sub v1 v3 in
  let v5 = add v2 v4 in
  let y = mul v5 v4 in
[ Bench.Test.create ~name:"graph_1" (fun () -> backward y v_1) ]
|> Bench.bench;;

let () =
  let open Graph in
  let x = variable 0.5 in
  let y = variable 1.5 in
  let b = sin x in
  let a =  mul x y in
  let z = add a b in
[ Bench.Test.create ~name:"graph_2" (fun () -> backward z x) ]
|> Bench.bench;;

let () =
  let open Backward in
  let t = init () in
  let t, x = var t 0.5 in
  let t, y = var t 4.2 in
  let t, b = sin t x in
  let t, a = mul t x y in
  let t, z = add t a b in
[ Bench.Test.create ~name:"backward" (fun () -> wrt (grad t z) x) ]
|> Bench.bench;;
