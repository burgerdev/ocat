
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

  let _ =
    Fmt.pr "Keeping track of a session using Functor:\n";
    login u p
    |> map Unix.gettimeofday
    |> time_to_database
end
