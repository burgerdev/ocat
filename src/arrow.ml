module type Arrow_base = sig
  type ('a, 'b) t

  val dimap: ('a -> 'b) -> ('b, 'c) t -> ('c -> 'd) -> ('a, 'd) t

  val id: ('a, 'a) t

  val compose: ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t

  (* TODO better name *)
  val first: ('a, 'b) t -> ('c * 'a, 'c * 'b) t

end

module type Arrow = sig
  include Arrow_base

  val contramap: ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val map:('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t

  val (<<<): ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t

  val second: ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  val split: ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  val swap: ('a * 'c, 'b * 'd) t -> ('c * 'a, 'd * 'b) t

  val chain: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val (>>>): ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  val zip: ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val observe: ('a, 'b) t -> ('a, 'c) t -> ('a, 'b) t
end

module Arrow (A: Arrow_base): Arrow with type ('a, 'b) t := ('a, 'b) A.t = struct
  include A

(* include Profunctor(A) *)

  let map f_ab g = dimap (fun x -> x) f_ab g

  let contramap f g_ab = dimap f g_ab (fun x -> x)

  let chain f g = compose g f
  let (>>>) = chain
  let (<<<) = compose

  let swap f_ab =
    let flip (x, y) = (y, x) in
    dimap flip f_ab flip

  let second f_ab = swap @@ first f_ab

  let split f_ab g_cd =
    first g_cd >>> second f_ab

  let zip f_ab f_ac = contramap (fun a -> (a, a)) (split f_ab f_ac)
  let observe f_ab eff_ac = map (zip f_ab eff_ac) fst
end
