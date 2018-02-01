open Functor

module type Comonad_base = sig
  type 'a t

  val extract: 'a t -> 'a
  val extend: ('a t -> 'b) -> 'a t -> 'b t
end

module type Comonad = sig
  include Comonad_base
  include Functor with type 'a t := 'a t

  val duplicate: 'a t -> 'a t t
end

module Comonad (B: Comonad_base): Comonad with type 'a t := 'a B.t = struct
  include B

  let map f a_m = extend (fun x -> extract x |> f) a_m

  include (Functor (
    struct
      type 'a t = 'a B.t
      let map = map
    end
    ))

  let duplicate a_m = extend (fun x -> x) a_m
end
