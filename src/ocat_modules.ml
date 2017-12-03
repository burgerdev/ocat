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
