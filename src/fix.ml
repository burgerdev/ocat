open Functor
open Cofree


module type Fix = sig
  type 'a t
  type 'a fix = Fix of ('a fix) t

  module C: Cofree with type 'a f := 'a t

  val fix: 'a fix t -> 'a fix
  val unfix: 'a fix -> 'a fix t

  val cata: ('a t -> 'a) -> 'b fix -> 'a
  val para: (('b fix * 'a) t -> 'a) -> 'b fix -> 'a
  val histo: ('a C.t t -> 'a) -> 'b fix -> 'a

  val ana: ('a -> 'a t) -> 'a -> 'b fix
  (* TODO apo *)
  (* TODO futu *)

  val hylo: ('a -> 'a t) -> ('b t -> 'b) -> 'a -> 'b

end

module Fix (F: Functor_base): Fix with type 'a t := 'a F.t = struct
  open F

  module C = Cofree(F)

  type 'a fix = Fix of ('a fix) t

  let fix x = Fix x
  let unfix = function Fix x -> x

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

  let histo c_alg a_fix =
    let rec aux a_fix =
      unfix a_fix
      |> map aux
      |> fun x -> C.Cofree (c_alg x, lazy x)
    in
    aux a_fix |> C.extract

end
