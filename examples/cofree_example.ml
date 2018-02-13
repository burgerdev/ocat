
open Ocat
open Ocat.Experimental
open Ocat.Ocat_modules

(* This example shows how to model a stream using the [Cofree] module. *)
module Stream_example = struct

  (* Our stream. *)
  module Stream = struct

    (* Our simple stream makes use of [option] as underlying functor. An
       instance of the stream is a pair of
       - a value and
       - a stream option
         * that's [None] if the stream is closed or
         * that's [Some] of the stream's tail.
    *)
    include Cofree(Option)

    (* Yes, that's all. Everything else in the module is just for convenience. *)

    (* [from n] generates the natural numbers starting from integer [n]. *)
    let rec from n =
      let next = lazy (Some (from (n+1))) in
      Cofree (n, next)

    (* [take n s] constructs a stream consisting of (at most) the first [n]
       elements of stream [s], returning [None] if there's no element at all. *)
    let rec take n = function Cofree (x, next) ->
      if n < 1 then
        None
      else
        Some (Cofree (x, Lazy.map (fun y -> Option.(y >>= take (n-1))) next))

    (* Fold the stream with a function that accumulates results. This will not
       terminate for infinite streams. *)
    let rec fold_left f s = function Cofree (x, next) ->
      let y = f s x in
      match Lazy.force next with
      | None -> y
      | Some c -> fold_left f y c

    (* Execute some side-effect for every item in the stream. *)
    let foreach f c =
      let g () x = f x in
      fold_left g () c

  end

  open Stream

  (* Note that creating [n] stack frames would probably throw an exception,
     thus this example returning implies that [Cofree] is stack-safe. *)
  let n = 1000000
  let _ =
    Fmt.pr "Stream example:\n";
    (* We start with all the natural numbers *)
    from 1
    (* and take only the first [n] items. *)
    |> take n
    (* Now calculate the sum of the first [n] integers by folding *)
    |> Option.map @@ fold_left (+) 0
    (* which can be done more efficient by applying Gauss' formula. *)
    |> Option.foreach @@ Fmt.pr "%d = %d\n" ((n * (n + 1)) / 2)

end
