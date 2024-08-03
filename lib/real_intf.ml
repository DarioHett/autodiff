module type Real_intf = sig
  type t

  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val log : t -> t
  val exp : t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val sinh : t -> t
  val cosh : t -> t
  val tanh : t -> t
  val neg : t -> t
  val to_string : t -> string
end
