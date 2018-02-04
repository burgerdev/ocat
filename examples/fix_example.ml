open Ocat

(* 'Adventures in Uncertainty' says that implementing Fibonacci numbers using a
   histomporphism would be a good excercise, so here it is.

   http://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/
*)
module Fix_example = struct
  module Nat = struct
    type 'a t =
      | Zero
      | Nat of 'a


    let rec map f = function
      | Zero -> Zero
      | Nat n -> Nat (f n)
  end

  include Nat
  include Fix(Nat)

  let nat_of_int_alg = function
    | 0 -> Zero
    | n when n > 0 -> Nat (n - 1)
    | _ -> failwith "only positive integers allowed"

  let nat_of_int i = ana nat_of_int_alg i

  let int_of_nat_alg = function
    | Zero -> 0
    | Nat n -> 1 + n

  let int_of_nat n = cata int_of_nat_alg n

  let fib_alg: (int C.t t) -> int = function
    | Zero ->
      Fmt.pr "Zero\n";
      1
    | Nat (Cofree (a, hist)) ->
      match Lazy.force hist with
      | Zero -> (* this is "fib 1" *)
        Fmt.pr "One\n";
        1
      | Nat (Cofree (b, _)) ->
        Fmt.pr "Summing %d + %d\n" a b;
        a + b

  let fib nat = histo fib_alg nat

  let rec naive_fib = function
    | 0 -> 1
    | 1 -> 1
    | n -> naive_fib (n - 1) + naive_fib (n - 2)

  let _ =
    let i = 12 in
    let nat = nat_of_int i in
    let elaborate = fib nat in
    let naive = naive_fib i in
    Fmt.pr "Naive: %d\nElaborate: %d\n" naive elaborate
end
