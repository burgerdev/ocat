
module type Functor_base = sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
end

module type Functor = sig
  include Functor_base

  val foreach: ('a -> unit) -> 'a t -> unit

  type 'a fix = Fix of ('a fix) t

  val fix: 'a fix t -> 'a fix
  val unfix: 'a fix -> 'a fix t

  val fixed_map: ('a -> 'b) -> 'a fix -> 'b fix

  val cata: ('a t -> 'a) -> 'a fix -> 'a

  val ana: ('a -> 'a t) -> 'a -> 'a fix

  val hylo: ('a -> 'a t) -> ('b t -> 'b) -> 'a -> 'b
end

module Functor (F: Functor_base): Functor with type 'a t := 'a F.t = struct
  include F

  let foreach f a = map f a |> ignore

  type 'a fix = Fix of ('a fix) t

  let fix x = Fix x
  let unfix = function Fix x -> x

  let rec fixed_map f a_fix =
    unfix a_fix |> map @@ fixed_map f |> fix

  let rec cata: ('a t -> 'a) -> 'a fix -> 'a = fun alg -> fun fix_x ->
    unfix fix_x
    |> map @@ cata alg
    |> alg

  let rec ana: ('a -> 'a t) -> 'a -> 'a fix = fun coalg -> fun x ->
    coalg x |> map (ana coalg) |> fix

  let rec hylo: ('a -> 'a t) -> ('b t -> 'b) -> 'a -> 'b = fun coalg alg -> fun x ->
    coalg x |> map (hylo coalg alg) |> alg
end
