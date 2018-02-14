
open Ocat

(* There's not that much to show about a plain [Functor] module, so don't
   expect to much in here. For a good introduction to the theoretical
   foundations I can heartily recommend Bartosz Milewski's lecture series on
   youtube: https://www.youtube.com/channel/UC8BtBl8PNgd3vWKtm2yJ7aA *)
module Context_example = struct

  (* Our example will focus on the interpretation of a functor as *context* for
     the values it holds - in this case, a user's login session. *)
  module Session: sig
    include Functor_base

    (* some boilerplate *)
    type user
    type password

    val user: string -> user
    val password: string -> password

    val pp_user: user Fmt.t
    val pp_password: password Fmt.t

    (* The [login] method starts a session. *)
    val login: user -> password -> unit t

    (* [user_of_session] can extract the user information from a session holding
       any data. *)
    val user_of_session: 'a t -> user
  end = struct
    type user = User of string
    type password = Password of string

    let user u = User u
    let password p = Password p

    let pp_user = Fmt.(using (function User s -> s) string)
    let pp_password = Fmt.(using (function Password _ -> "*****") string)

    type 'a t = Session of 'a * user

    let login u p =
      (* TODO authenticate the user :) *)
      Fmt.pr "Successful login: [%a:%a]\n" pp_user u pp_password p;
      Session ((), u)

    let user_of_session = function
      | Session (x, u) -> u

    let map f = function
      | Session (x, u) -> Session (f x, u)
  end

  open Session
  include Functor (Session)

  let (u, p) = (user "admin", password "changeme!")

  let print_login_attempt: float t -> unit = fun session ->
    let u = user_of_session session in
    let log f = Fmt.pr "[%0.f] User '%a' logged in.\n" f pp_user u in
    foreach log session

  let _ =
    Fmt.pr "Keeping track of a session using Functor:\n";
    login u p
    |> map Unix.gettimeofday
    |> print_login_attempt
end
