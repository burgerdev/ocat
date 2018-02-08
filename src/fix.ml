open Functor
open Cofree

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

module type Fix = sig
  type 'a t
  type 'a fix = Fix of ('a fix) t

  module C: Cofree with type 'a f := 'a t

  val fix: 'a fix t -> 'a fix
  val unfix: 'a fix -> 'a fix t

  val cata: ('a t -> 'a) -> 'b fix -> 'a
  val para: (('b fix * 'a) t -> 'a) -> 'b fix -> 'a
  val histo: ('a C.t t -> 'a) -> 'b fix -> 'a
  val zygo: ('b t -> 'b) -> (('b * 'a) t -> 'a) -> 'c fix -> 'a

  val ana: ('a -> 'a t) -> 'a -> 'b fix
  val apo: ('a -> ('b fix, 'a) either t) -> 'a -> 'b fix
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

  let rec apo p_coalg a =
    let aux = function
      | Left a_fix -> a_fix
      | Right a -> (apo p_coalg) a
    in
    p_coalg a |> map aux |> fix

  let histo c_alg a_fix =
    let rec aux a_fix =
      unfix a_fix
      |> map aux
      |> fun x -> C.Cofree (c_alg x, lazy x)
    in
    aux a_fix |> C.extract

  let zygo alg pair_alg a_fix =
    let rec aux a_fix =
      let f_a = unfix a_fix in
      let mapped = map aux f_a in
      let b = mapped |> map fst |> alg in
      let a = pair_alg mapped in
      (b, a)
    in
    aux a_fix
    |> snd

end
