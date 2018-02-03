
open Ocat

module Context_example = struct

  module type Session_sig = sig
    include Functor_base

    type user
    type password

    val user: string -> user
    val password: string -> password

    val pp_user: user Fmt.t
    val pp_password: password Fmt.t

    val login: user -> password -> unit t
    val user_of_session: 'a t -> user
  end
  module Session: Session_sig = struct
    type user = User of string
    type password = Password of string

    let user u = User u
    let password p = Password p

    let pp_user = Fmt.(using (function User s -> s) string)
    let pp_password = Fmt.(using (function Password _ -> "*****") string)

    type 'a t = Session of 'a * user

    let login u p =
      (* TODO authenticate the user *)
      Fmt.pr "Successful login: [%a:%a]\n" pp_user u pp_password p;
      Session ((), u)

    let user_of_session = function
      | Session (x, u) -> u

    let map f = function
      | Session (x, u) -> Session (f x, u)
  end

  include Session
  include Functor (Session)

  let (u, p) = (user "admin", password "changeme!")

  let time_to_database: float t -> unit = fun session ->
    let u = user_of_session session in
    let log f = Fmt.pr "Writing timestamp %.0f to user table of '%a'.\n" f pp_user u in
    foreach log session

  let _ = login u p
          |> map Unix.gettimeofday
          |> time_to_database
end

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

  (* No demo without some console output. *)
  let _ =
    let e = mul (const 3) (const 2) |> add @@ const 2 |> mul @@ const 5 in
    let v = eval e in
    let s = string_of_expr e in
    let s' = expand_string v in
    Fmt.pr "%s = %d = %s\n" s v s'

  (* If you made it to this point, it's time for some weird stuff. Let's try to
     split [expand_string] into its constituents.
  *)

  (* This functions expands an [int] to an equivalent arithmetical expression.
     Note that the type argument to [fix] is indeed fixed, where it was free in
     earlier examples, where we defined the [expr] object directly. *)
  let expand: int -> int fix = fun n -> ana expand_coalg n

  (* Reminder: we defined this function above. *)
  let string_of_expr: string fix -> string = fun e -> cata string_alg e

  (* It should be fairly obvious that those two functions do not compose. The
     trick is that the so-called 'carrier type' is only virtual in our case, and
     we can force the compiler to convert between carrier types.
  *)
  let expand_string' n =
    let e = expand n in
    let rec swap_carrier f x =
      unfix x
      |> map @@ swap_carrier f
      |> fix in
    let e' = swap_carrier string_of_int e in
    string_of_expr e'

  let _ =
    let n = 420 in
    let s = expand_string' n in
    Fmt.pr "%s = %d\n" s n
end
