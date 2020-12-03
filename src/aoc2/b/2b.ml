open Core
open Stdio
open Angstrom

let ( let* ) = (>>=)

let num = take_while1 Char.is_digit >>| Int.of_string

let single_policy =
  let* lb = num <* char '-' in
  let* ub = num <* char ' ' in
  let* c = any_char <* string ": " in
  let* password = take_while Char.is_alpha in
  let test x = Char.equal c (String.get password (x - 1)) in

  return ((test lb && not (test ub)) || (not (test lb) && test ub))

let password_philosophy input =
  let parser = sep_by (char '\n') single_policy in
  match Angstrom.parse_string ~consume:Consume.Prefix parser input with
  | Error _ -> raise (Invalid_argument "Invalid input")
  | Ok list -> List.count ~f:Fn.id list

let () =
  In_channel.create "./src/aoc2/input.txt"
  |> In_channel.input_all
  |> password_philosophy
  |> Stdio.printf "%d\n"