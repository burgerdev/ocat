open OUnit
open Fmt

open Ocat
open Ocat.Ocat_modules

module type Test = sig
  module M: Monad

  val name: string
  val cmp: 'a M.t -> 'a M.t -> bool
end

let seed name =
  Random.self_init ();
  let seed = Random.bits () in
  Printf.eprintf "%s: Seeding with %d\n" name seed;
  Random.init seed

type my_t = Left | Middle | Right

let test_base: (module Test) -> OUnit.test =
  fun t ->
    let module T = (val t) in
    let open T.M in
    let f = function Left -> return Middle | _ -> assert false in
    let g = function Middle -> return Right | _ -> assert false in
    T.name >:: fun _ ->
      assert_equal ~cmp:T.cmp ~msg:"return is not a left identity"
        (return Left >>= f) (f Left);
      assert_equal ~cmp:T.cmp ~msg:"return is not a right identity"
        (return Right >>= return) (return Right);
      assert_equal ~cmp:T.cmp ~msg:"bind is not associative"
        (return Left >>= f >>= g) (return Left >>= fun x -> f x >>= g)


module Id_test: Test = struct
  module M = Id
  let name = "test_id"
  let cmp x y = x = y
end

module Option_test: Test = struct
  module M = Option

  let name = "test_option"
  let cmp x y = match (x, y) with
    | (None, None) -> true
    | (Some x, Some y) when x = y -> true
    | _ -> false
end

module List_test: Test = struct
  module M = List

  let name = "test_list"
  let cmp a b =
    List.compare_lengths a b = 0 &&
    List.for_all (fun (x, y) -> x = y) (List.combine a b)
end


let tests =
  [ (module Id_test: Test)
  ; (module Option_test: Test)
  ; (module List_test: Test)
  ] |> List.map test_base

let suite =
  "monoid suite" >::: tests

let _ =
  run_test_tt_main suite
