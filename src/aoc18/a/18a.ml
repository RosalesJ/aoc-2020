open Core
open Angstrom

let operation_order input =
  let parens p = char '(' *> p <* char ')' in
  let add = string " + " *> return (+) in
  let sub = string " - " *> return (-) in
  let mul = string " * " *> return ( * ) in
  let div = string " / " *> return (/) in
  let integer = take_while1 Char.is_digit >>| Int.of_string in

  let chainl1 e op =
    let rec go acc =
      (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
    e >>= fun init -> go init
  in

  let expr = fix (fun expr ->
    let factor = parens expr <|> integer in
    chainl1 factor (mul <|> div <|> add <|> sub))
  in

  match parse_string (sep_by end_of_line expr) input ~consume:All with
  | Ok v -> List.sum (module Int) v ~f:ident
  | Error e -> failwith e

let () =
  In_channel.create "./src/aoc18/input.txt"
  |> In_channel.input_all
  |> operation_order
  |> Format.printf "%d\n"