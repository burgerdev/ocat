open Monoid
open Monad

module type MonadT = sig
  include Monad
  module M: Monad

  val lift: 'a M.t -> 'a t
end

module OptionT (M: Monad): MonadT with type 'a M.t := 'a M.t and type 'a t := 'a option M.t = struct
  module M = M

  module N = struct
    type 'a t = 'a option M.t

    let return x = M.return (Some x)
    let bind: type a b. (a -> b t) -> a t -> b t = fun f m ->
      M.(m >>= function
        | None -> return None
        | Some x -> f x)

  end

  include Monad (N)
  let lift m = M.(map (fun x -> Some x) m)
end

module ResultT (E: sig type e end) (M: Monad): MonadT with type 'a M.t := 'a M.t and type 'a t := ('a, E.e) result M.t = struct
  module M = M

  module N = struct
    type 'a t = ('a, E.e) result M.t

    let return x = M.return (Ok x)
    let bind: type a b. (a -> b t) -> a t -> b t = fun f m ->
      M.(m >>= function
        | Error _ as e -> return e
        | Ok x -> f x)
  end

  include Monad (N)
  let lift m = M.(map (fun x -> Ok x) m)
end


module type WriterT = sig
  module L: Monoid
  type log = L.t

  module M: Monad
  type 'a m = 'a M.t

  include MonadT with type 'a t = ('a * log) m and module M := M

  val tell: log -> 'a t -> 'a t
  val written: 'a t -> log m
  val value: 'a t -> 'a m
end

module WriterT (Log: Monoid_base) (M: Monad_base): WriterT with type 'a M.t := 'a M.t and type L.t := Log.t = struct
  module L = Monoid(Log)
  type log = Log.t
  type 'a m = 'a M.t

  module M = Monad (M)

  module N = struct
    type 'a t = ('a * log) m

    let bind: ('a -> 'b t) -> 'a t -> 'b t = fun f a_t -> M.(a_t >>= function
      | (a, log) -> f a |> map @@ fun (b, log') -> (b, L.(log |+| log'))
      )
    let return: 'a -> 'a t = fun a -> M.return (a, L.zero)
  end

  include N
  include Monad(N)

  let lift: 'a m -> 'a t = fun m -> M.map (fun a -> (a, L.zero)) m

  let tell log m = m |> M.map @@ function (a, log') -> (a, L.(log' |+| log))
  let written m = M.(map snd m)
  let value m = M.(map fst m)
end
