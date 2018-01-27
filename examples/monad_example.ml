
open Ocat

(* Other examples are all about demystifying monads. Here, we are going to add
   some more magic to the mix!
*)
module Magic_example = struct

  (* The [Magic] module encapsulates magic tricks that depend on a magic word.
     In muggle society, this is known as the [Reader] monad. *)
  module Magic = struct
    type 'a t = Conjure of (word -> 'a)
    and word =
      | Presto
      | Abracadabra

    (* This convenience function makes a Conjuration appear from nowhere. *)
    let conjure f = Conjure f

    (* A spell is woven by saying the magic word. *)
    let say inc = function Conjure f -> f inc

    (* Below is the profane boilerplate we need to write to get a free [Monad] module *)

    let return a = Conjure (fun _ -> a)

    let bind g = function Conjure f ->
      Conjure (fun i -> f i |> g |> function Conjure h -> h i)
  end

  include Magic
  include Monad (Magic)

  let etymology = conjure @@ function
    | Presto -> "italian"
    | Abracadabra -> "(maybe) aramaic"

  let describe n =
    conjure @@ function
    | Presto -> Fmt.pr "%d flowers appear when using 'Presto'" n
    | Abracadabra -> Fmt.pr "%d rabbits appear when using 'Abracadabra'" n

  let magic_show word =
    (* We start with some string - nothing up my sleeves! *)
    return "3"
    (* Sometimes we just need some profane functions. *)
    |> map int_of_string
    (* The [describe] function wants to know the magic word. *)
    >>= describe
    (* Now let's scrutinize the magic word. *)
    >>= (fun _ -> etymology)
    (* Note how the printing logic is entirely orthogonal to magic. *)
    |> map (fun origin -> Fmt.pr ", which is %s.\n" origin)
    (* Finally, we start the show by saying the magiv word. *)
    |> say word

  let _ =
    Fmt.pr "Magic monadic reader:\n";
    magic_show Presto;
    magic_show Abracadabra
end
