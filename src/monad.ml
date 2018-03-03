open Functor
open Arrow

module type Monad_base = sig
  type 'a t

  val return: 'a -> 'a t
  val bind: ('a -> 'b t) -> 'a t -> 'b t
end

module type Monad = sig
  include Monad_base
  include Functor with type 'a t := 'a t

  val join: 'a t t -> 'a t
  val ap: ('a -> 'b) t -> 'a t -> 'b t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val (>=>): ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

  module Kleisli: sig
    include Arrow with type ('a, 'b) t := 'a -> 'b t

    val ask: 'a -> ('a -> 'b t) -> 'b t
    val local: ('a -> 'b) -> ('b -> 'c t) -> 'a -> 'c t
  end
end

module Monad (B: Monad_base): Monad with type 'a t := 'a B.t = struct
  include B

  let (>>=) m f = bind f m
  let map f a_m = a_m >>= fun x -> return @@ f x

  include (Functor (
    struct
      type 'a t = 'a B.t
      let map = map
    end
    ))

  let join a_m_m = a_m_m >>= fun x -> x
  let ap f_m a_m = f_m >>= (fun f -> map f a_m)
  let (>=>) f g = fun x -> f x >>= g

  module Kleisli_base: Arrow_base with type ('a, 'b) t = 'a -> 'b t = struct
    type ('a, 'b) t = 'a -> 'b B.t

    let dimap f r g = fun a ->
      f a |> r |> map g

    let id = fun x -> return x

    let dimap f r g = fun a -> f a |> r |> map g

    let compose a_bc a_ab = fun a -> a_ab a >>= a_bc

    let first a_bc = fun (a, b) -> a_bc b >>= (fun c -> return (a, c))
  end

  module Kleisli = struct
    include Kleisli_base
    include Arrow (Kleisli_base)

    let ask a f = f a
    let local = contramap
  end
end
