open Ocat

(* The rational numbers with addition form a group, and every group is also a
   monoid.
*)
module Rational_example = struct

  module Rational: sig
    include Monoid_base
    val rational: int * int -> t
    val pp: t Fmt.t
    val simplify: t -> t
  end = struct
    type t = int * int

    (* We hide the underlying type and apply some sanity checks to user
       provided input.
    *)
    let rational = function
      | (_, d) when d <= 0 -> failwith "denominator must be positive"
      | (n, d) -> (n, d)

    (* This naive method tries to cancel the fraction with the first couple of
       primes.
    *)
    let simplify (a, b) =
      let rec aux (a, b) = function
        | [] -> (a, b)
        | h :: t when a mod h = 0 && b mod h == 0 ->
          aux (a / h, b / h) (h :: t)
        | _ :: t -> aux (a, b) t
      in aux (a, b) [2; 3; 5; 7; 11; 13]

    (* We arbitrarily choose 1 as denominator for the [zero] fraction. *)
    let zero = (0, 1)

    (* For adding to fractions, we first do the 'naive' addition and then try
       to simplify the result.
    *)
    let sum (a, b) (x, y) =
      (a * y + x * b, b * y) |> simplify

    (* A pretty printer for rational numbers, that catches the special cases
       - zero numerator
       - "one" in disguise
    *)
    let pp ppf r = match r with
      | (0, _) -> Fmt.int ppf 0
      | (a, b) when a = b -> Fmt.int ppf 1
      | r -> Fmt.(pair ~sep:(const string "/") int int) ppf r
  end

  include Rational
  include Monoid (Rational)

  (* Now that we defined a big module, we see that including the Monoid module
     buys us nothing (besides a pretty infix symbol). However, the Monoid module
     will be useful later on, when we can use it to parametrize the [Writer]
     module. Let's just wrap up with some arithmetic exercises.
  *)

  let r = rational (1, 2)
  let s = rational (2, 3)
  let neg_r = rational (-1, 2)

  let log_add r s = Fmt.pr "%a + %a = %a\n" pp r pp s pp (r |+| s)

  let _ =
    Fmt.pr "Rational numbers form a monoid:\n";
    log_add zero zero;
    log_add r s;
    log_add r neg_r;
    log_add r r;
    log_add s s
end


(* Instead of the obligatory examples of string or list concatenation, let's do
   some monoidal pretty-printing.
*)
module Fmt_example = struct
  module Fmt_monoid = struct
    type t = unit Fmt.t
    let zero = Fmt.nop
    let sum = Fmt.prefix
  end

  include Fmt_monoid
  include Monoid (Fmt_monoid)

  let str s = Fmt.(const string s)
  let int i = Fmt.(const int i)

  (* Arithmetics, again. *)
  let pp = int 1 |+| str " + " |+| int 2 |+| str " = " |+| int 3 |+| Fmt.cut

  let _ =
    Fmt.pr "Monoidal formatting:\n";
    pp Fmt.stdout ()
end

(* It is probably also interesting to see a counter example. *)
module Counter_example = struct
  (* The following module is not a monoid. Do you see why? *)
  module No_monoid = struct
    include Fmt_example.Fmt_monoid
    let sum a b = Fmt.(prefix a b |> parens)
  end

  include No_monoid
  include Monoid (No_monoid)

  let str s = Fmt.(const string s)

  (* The monoidal sum operation must be associative, which is not the case with
     our parenthesized pretty printer.
  *)
  let pp_1 = (str "a" |+| str "b") |+| str "c"
  let pp_2 = str "a" |+| (str "b" |+| str "c")

  let _ =
    Fmt.pr "Not a monoid:\n";
    Fmt.pr "%a != %a\n" pp_1 () pp_2 ()

end
