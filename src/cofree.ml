
open Comonad
open Functor
open Ocat_modules

module type Cofree = sig
  type 'a f
  type 'a t = Cofree of 'a * 'a t f Lazy.t

  include Comonad with type 'a t := 'a t
end

module Cofree (F: Functor_base): Cofree with type 'a f := 'a F.t = struct

  module M = struct
    type 'a t = Cofree of 'a * 'a t F.t Lazy.t

    let extract = function
      | Cofree (a, _) -> a

    let rec extend f = function
      | Cofree (a, tail) as c ->
        Cofree (f c, Lazy.(map F.(map (extend f)) tail))
  end

  include Comonad(M)
  include M

end
