open Functor
open Cofree


module type Fix = sig
  type 'a t

  type 'a algebra = 'a t -> 'a
  type 'a coalgebra = 'a -> 'a t

  type 'a fix = Fix of ('a fix) t

  module C: Cofree with type 'a f := 'a t

  val fix: 'a fix t -> 'a fix
  val unfix: 'a fix -> 'a fix t

  val fixed_map: ('a -> 'b) -> 'a fix -> 'b fix

  val cata: 'a algebra -> 'a fix -> 'a
  val para: (('a fix * 'a) t -> 'a) -> 'a fix -> 'a
  val histo: ('a C.t t -> 'a) -> 'a fix -> 'a

  val ana: 'a coalgebra -> 'a -> 'a fix
  (* TODO apo *)
  (* TODO futu *)

  val hylo: 'a coalgebra -> 'b algebra -> 'a -> 'b

end

module Fix (F: Functor_base): Fix with type 'a t := 'a F.t = struct
  open F

  module C = Cofree(F)

  type 'a algebra = 'a t -> 'a
  type 'a coalgebra = 'a -> 'a t

  type 'a fix = Fix of ('a fix) t

  let fix x = Fix x
  let unfix = function Fix x -> x

  let rec fixed_map f a_fix =
    unfix a_fix |> map @@ fixed_map f |> fix

  let rec cata alg a_fix =
    unfix a_fix
    |> map @@ cata alg
    |> alg

  let rec ana coalg a =
    coalg a |> map (ana coalg) |> fix

  let rec hylo coalg alg a =
    coalg a |> map (hylo coalg alg) |> alg

  let rec para p_alg a_fix =
    unfix a_fix
    |> map (fun a -> (a_fix, para p_alg a))
    |> p_alg

  let rec histo c_alg a_fix =
    let rec aux a_fix =
      unfix a_fix
      |> map aux
      |> fun x -> C.Cofree (c_alg x, lazy x)
    in
    aux a_fix |> C.extract

end
