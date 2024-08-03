open Real_intf

module Float : Real_intf with type t = float = struct
  include Core.Float

  let mul = Core.Float.( * )
  let div = Core.Float.( / )
  let pow = Core.Float.( ** )
end
