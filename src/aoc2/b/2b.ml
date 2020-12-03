open Core
open Stdio
open Angstrom

let test_policy m n  c password =
  let m = Char.equal c (String.get password (m - 1)) in
  let n = Char.equal c (String.get password (n - 1)) in
  (m || n) && not (m && n)

let single_policy =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  lift4 test_policy
    (num <* char '-')
    (num <* char ' ')
    (any_char <* string ": ")
    (take_while Char.is_alpha)

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