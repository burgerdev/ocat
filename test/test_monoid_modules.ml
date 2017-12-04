open OUnit
open Fmt

open Ocat
open Ocat.Ocat_modules


module type Test = sig
  module M: Monoid

  val random: 'a -> M.t
  val num_instances: int
  val name: string
  val cmp: M.t -> M.t -> bool
end

let seed name =
  Random.self_init ();
  let seed = Random.bits () in
  Printf.eprintf "%s: Seeding with %d\n" name seed;
  Random.init seed

let test_base: (module Test) -> OUnit.test =
  fun m ->
    let module M = (val m) in
    M.name >:: fun _ ->
      seed M.name;
      let examples = List.init M.num_instances M.random in

      let test_zero x =
        assert_equal ~cmp:M.cmp ~msg:"zero is not a right identity" M.M.(x |+| zero) x;
        assert_equal ~cmp:M.cmp ~msg:"zero is not a left identity" M.M.(zero |+| x) x
      in
      List.iter test_zero examples;

      let left = M.M.(List.fold_left (|+|) zero examples) in
      let right = M.M.(List.fold_right (|+|) examples zero) in
      assert_equal ~cmp:M.cmp ~msg:"sum is not associative" left right

module Int_test: Test = struct
  module M = Int

  let random _ =
    let i = Random.bits () in
    if Random.bool () then i else -i

  let num_instances = 10000
  let name = "test_int"
  let cmp x y = x - y = 0
end

module Float_test: Test = struct
  module M = Float

  let random _ = Random.float 1.0 -. 0.5

  let num_instances = 10000
  let name = "test_float"
  let cmp x y = abs_float (x -. y) < 1e15
end

module String_test: Test = struct
  module M = String

  let gen_char _ = Random.int 253 + 1 |> char_of_int

  let gen_float _ = Random.float 1.0 -. 0.5

  let random _ =
    let max_size = 10 in
    let size = Random.int max_size in
    String.init size gen_char

  let num_instances = 100
  let name = "test_string"
  let cmp a b = String.compare a b = 0
end


let tests =
  [ (module Int_test: Test)
  ; (module String_test: Test)
  ; (module Float_test: Test)
  ] |> List.map test_base

let suite =
  "monoid suite" >::: tests

let _ =
  run_test_tt_main suite
