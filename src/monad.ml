module type Monad_base = sig
  type 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  include Monad_base

  val join: 'a t t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
end

module Monad (B: Monad_base): Monad with type 'a t = 'a B.t  = struct
  include B

  let map f a_m = bind a_m @@ fun x -> return @@ f x
  let join a_m_m = bind a_m_m @@ fun x -> x
  let (>>=) = bind
end
