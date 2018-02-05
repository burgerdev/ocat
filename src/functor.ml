
module type Functor_base = sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
end

module type Functor = sig
  include Functor_base

  val foreach: ('a -> unit) -> 'a t -> unit
  val (>|=): 'a t -> ('a -> 'b) -> 'b t
end

module Functor (F: Functor_base): Functor with type 'a t := 'a F.t = struct
  include F

  let foreach f a = map f a |> ignore

  let (>|=) a f = map f a
end
