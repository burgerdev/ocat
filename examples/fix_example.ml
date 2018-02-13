open Ocat

(*
   A classical example for fixed-point recursion, see e.g.
   https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
*)
module Expr_example = struct

  (* This type is an ADT for building expressions in a rudimentary calculator
     language. Expressions are either integer constants, or arithmetic
     combinations (plus, times) of expressions.
  *)
  type 'a expr = Const of int
               | Add of 'a * 'a
               | Mul of 'a * 'a

  (* We define some basics and let [Ocat.Functor] derive the fancy things. *)
  module Expr: Functor_base with type 'a t = 'a expr = struct
    type 'a t = 'a expr

    let map f = function
      | Const i -> Const i
      | Add (l, r) -> Add (f l, f r)
      | Mul (l, r) -> Mul (f l, f r)
  end

  include Functor (Expr)
  include Fix (Expr)

  (* Below methods are convenience type constructors which handle the [Fix]
     part for us.
  *)
  let const i = Const i |> fix
  let add i j = Add (i, j) |> fix
  let mul i j = Mul (i, j) |> fix

  (* Subsequent definitions show how to create an F-(co)algebra and how to
     apply it to an expression.
  *)

  (* This F-algebra derives a string representation of an [expr]. Since we only
     deal with a small set of expressions, parenthesizing additions already
     yields an equivalent string you could plug into utop, octave, etc.
     Note that there's no recursion involved, we just pretend that the recursive
     parts are already processed (i.e. [l] and [r] are already strings).
  *)
  let string_alg = function
    | Const i -> string_of_int i
    | Add (l, r) -> Fmt.strf "(%s + %s)" l r
    | Mul (l, r) -> Fmt.strf "%s * %s" l r

  (* This is how to apply an algebra to a [fix]ed expression. [cata] handles the
     recursion, [string_alg] handles the business logic.
  *)
  let string_of_expr e = cata string_alg e

  (* This is the actual calculator implementation. *)
  let eval_alg = function
    | Const i -> i
    | Add (l, r) -> l + r
    | Mul (l, r) -> l * r

  (* [eval] evaluates the expression recursively, yielding an [int]. *)
  let eval e = cata eval_alg e


  (* This co-algebra describes how to generate an [expr] from a given [int], or,
     more accurately, breaks down an integer into computations that only involve
     'small' numbers. Note that we're not very careful and ignore the fact that
     input might be negative, and that this example is a bot contrived.
  *)
  let expand_coalg = function
    | i when i < 6 -> Const i
    | i when i mod 2 = 0 -> Mul (2, i / 2)
    | i -> Add (5, i - 5)

  (* This function creates an extended string represenation (a small formula)
     for a given integer. It uses [hylo], which simultaneously applies the
     anamorphic part [expand_coalg] and the catamorphic part [string_alg].
     We could have used the co-algebra with [ana], but [hylo] is arguably better
     for demonstration - result is a string which we just have to print.
  *)
  let expand_string n = hylo expand_coalg string_alg n

  let _ = Fmt.pr "Functors and algebras:\n"

  (* No demo without some console output. *)
  let _ =
    let e = mul (const 3) (const 2) |> add @@ const 2 |> mul @@ const 5 in
    let v = eval e in
    let s = string_of_expr e in
    let s' = expand_string v in
    Fmt.pr "%s = %d = %s\n" s v s'

  (* This functions expands an [int] to an equivalent arithmetical expression.
     Note that the type argument in the result is free, so we can feed it to
     another function with an unrelated carrier type.
  *)
  let expand: int -> 'a fix = fun n -> ana expand_coalg n

  let expand_string' n = expand n |> string_of_expr

  let _ =
    let n = 420 in
    let s = expand_string' n in
    Fmt.pr "%s = %d\n" s n
end

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

  let fib_alg: (int Cofree_F.t t) -> int = function
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
    Fmt.pr "Calculate fibonacci numbers with fixed-point recursion:\n";
    let i = 12 in
    let nat = nat_of_int i in
    let elaborate = fib nat in
    let naive = naive_fib i in
    Fmt.pr "Naive: %d\nElaborate: %d\n" naive elaborate
end
