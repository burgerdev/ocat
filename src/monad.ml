open Functor

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
  let ap: ('a -> 'b) t -> 'a t -> 'b t = fun f_m a_m ->
    f_m >>= (fun f -> map f a_m)
  let (>=>) f g = fun x -> f x >>= g
end
