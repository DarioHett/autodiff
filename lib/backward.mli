type var =
  { value : float
  ; index : int
  }

val string_of_var : var -> string

type 'a node = Node of 'a * 'a

val weights : var node -> float * float
val deps : var node -> int * int

type tape = var node list

val init : unit -> tape
val string_of_tape : tape -> string
val push0 : tape -> var node list * int
val push1 : tape -> var -> var node list * int
val push2 : tape -> var -> var -> var node list * int
val var : tape -> float -> tape * var
val sin : tape -> var -> var node list * var
val add : tape -> var -> var -> var node list * var
val mul : tape -> var -> var -> var node list * var

type grad = Grad of float array

val grad : tape -> var -> grad
val wrt : grad -> var -> float
val string_of_grad : grad -> string
