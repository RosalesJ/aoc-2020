open Core
open Angstrom

type basic = [`Single of int | `Double of (int * int)]
type typ = [basic | `Char of char | `Alternate of basic * basic]
type rule = { num: int; typ: typ}

let make_rule num typ = { num; typ }

let parse_input input =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  let single = num >>| fun x -> `Single x in
  let double = lift2 (fun x y -> `Double (x, y)) (num <* char ' ') num in
  let char_ = char '"' *> any_char <* char '"' >>| fun x -> `Char x in
  let basic = double <|> single in
  let alternate = lift2 (fun x y -> `Alternate (x, y)) (basic <* string " | ") basic in
  let rule = lift2 make_rule (num <* string ": ") (char_ <|> alternate <|> double <|> single) in
  let messages = sep_by end_of_line (take_while1 Char.is_alpha) in
  let parser = lift2 Tuple2.create (sep_by end_of_line rule <* string "\n\n") messages in
  match parse_string parser input ~consume:All with
  | Ok (rules, messages) -> rules, messages
  | Error e -> failwith e

let build_parser rules =
  let rules = List.map rules ~f:(fun rule -> (rule.num, rule)) |> Map.of_alist_exn (module Int) in
  let rec loop next =
    let rule = Map.find_exn rules next in
    match rule.typ with
    | `Char c -> char c *> return ()
    | `Single n -> loop n
    | `Double (m, n) -> loop m *> loop n
    | `Alternate (`Single m, `Single n) -> loop m <|> loop n
    | `Alternate (`Double (x, y), `Double (z, w)) -> (loop x *> loop y) <|> (loop z *> loop w)
    | _ -> failwith "Bakana!"
  in
  loop 0

let monster_messages input =
  let rules, _messages = parse_input input in
  let parser = build_parser rules in

  List.map ~f:(parse_string parser ~consume:All) _messages
  |> List.count ~f:(function Ok _ -> true | _ -> false)

let () =
  In_channel.create "./src/aoc19/input.txt"
  |> In_channel.input_all
  |> monster_messages
  |> Format.printf "%d\n"