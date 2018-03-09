
(** Monoids *)

open Monoid

module Int = struct
  type t = int
  include Monoid(struct
      type t = int
      let zero = 0
      let sum = (+)
    end)
end

module String = struct
  include (Monoid(struct
             type t = string
             let zero = ""
             let sum = (^)
           end): Monoid with type t := string)
  include String
end

(** Monads *)

open Monad
open Comonad

module Id = struct
  type 'a t = 'a
  include Monad (
    struct
      type 'a t = 'a
      let return x = x
      let bind f x = f x
    end)
  include Comonad (
    struct
      type 'a t = 'a
      let extract x = x
      let extend f x = f x
    end)
end

module Option = struct
  type 'a t = 'a option
  include Monad (
    struct
      type 'a t = 'a option
      let return x = Some x
      let bind f x = match x with Some v -> f v | None -> None
    end)

  let of_list = function
    | h :: t -> Some h
    | [] -> None

  let default value = function
    | Some x -> x
    | None -> value
end

module List = struct
  type 'a t = 'a list
  include Monad (
    struct
      type 'a t = 'a list
      let return x = [x]
      let bind f x = List.(map f x |> flatten)
    end)
  include List

  let of_option = function
    | Some x -> [x]
    | None -> []
end

module Lazy = struct
  include Lazy
  include Monad (
    struct
      type 'a t = 'a Lazy.t
      let return x = Lazy.from_val x
      let bind f x = lazy (Lazy.force x |> f |> Lazy.force)
    end)
  include Comonad (
    struct
      type 'a t = 'a Lazy.t
      let extract x = Lazy.force x
      let extend f x = lazy (f x)
    end)

  let foreach f l =
    Lazy.force l |> f
end

module Reader = Id.Kleisli
