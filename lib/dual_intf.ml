module type Dual_intf = sig
  type r
  type d
  type t

  val constant : r -> t
  val variable : r -> t
  val exp : t -> t
  val derivative : (t -> t) -> t -> d
end
