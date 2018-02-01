
open Monad
open Functor

module type Free = sig
  type 'a f
  type 'a t =
    | Val of 'a
    | Suspend of 'a t f

  include Monad with type 'a t := 'a t
end

module Free (F: Functor_base): Free with type 'a f := 'a F.t = struct

  module M = struct
    type 'a t =
      | Val of 'a
      | Suspend of 'a t F.t

    let return x = Val x
    let rec bind f = function
      | Val a -> f a
      | Suspend tail ->
        let g: 'a t -> 'b t = fun a_t -> bind f a_t in
        Suspend F.(map g tail)
  end

  include Monad(M)

  include M

end
