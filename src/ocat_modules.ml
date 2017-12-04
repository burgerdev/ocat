open Ocat

(** Monoids *)

module Int = Monoid(struct
    type t = int
    let zero = 0
    let sum = (+)
  end)

module String = struct
  include (Monoid(struct
    type t = string
    let zero = ""
    let sum = (^)
  end): Monoid with type t := string)
  include String
end

module Float = Monoid(struct
    type t = float
    let zero = 0.0
    let sum = (+.)
  end)

(** Monads *)

module Id = Monad (
  struct
    type 'a t = 'a
    let return x = x
    let bind x f = f x
  end)

module Option = Monad (
  struct
    type 'a t = 'a option
    let return x = Some x
    let bind x f = match x with Some v -> f v | None -> None
  end)

module List = struct
  include Monad (
    struct
      type 'a t = 'a list
      let return x = [x]
      let bind x f = List.(map f x|> flatten)
    end)
  include List
end
