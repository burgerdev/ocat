
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

module Float = struct
  type t = float
  include Monoid(struct
      type t = float
      let zero = 0.0
      let sum = (+.)
    end)
end

(** Monads *)

open Monad

module Id = struct
  type 'a t = 'a
  include Monad (
    struct
      type 'a t = 'a
      let return x = x
      let bind f x = f x
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
end

module List = struct
  type 'a t = 'a list
  include Monad (
    struct
      type 'a t = 'a list
      let return x = [x]
      let bind f x = List.(map f x|> flatten)
    end)
  include List
end
