open OUnit
open Fmt

open Ocat
open Ocat.Ocat_modules
open Ocat.Ocat_transformers

module I64 = struct
  include Int64
  let (<+) x y = add x (of_int y)
end

type data = conn * age
and conn = Connection of string
and age =
  | Age_unix of int64
  | Age of years * days
and years = Years of int
and days = Days of int

module X = ReaderT (struct type ask = data end) (Id)

include ReaderT (struct type ask = data end) (Id)

let modify_connection (Connection s, age) = Connection ("modified: " ^ s)

let modify_age conn = fun (_, age) -> let age = match age with
    | Age_unix ts -> Age_unix I64.(ts <+ 24 * 60 * 60 * 1000)
    | Age (y, Days d) -> Age (y, Days (d+1))
  in (conn, age)

let make_string (Connection conn, age) = match age with
  | Age_unix ts -> Fmt.strf "%s: %Ldms since epoch" conn ts
  | Age (Years y, Days d) -> Fmt.strf "%s: %d years, %d days" conn y d

let chained = modify_connection >>= modify_age |> map make_string

let result = ask (Connection "foo", Age_unix (I64.of_int 0)) chained
