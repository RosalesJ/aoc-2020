open Core
open Angstrom

type rule = { name: string; lo_range: int * int; hi_range: int * int }
type ticket = int list
type data = { rules: rule list; my_ticket: ticket; tickets: ticket list }

let make_rule name lo_range hi_range = { name; lo_range; hi_range }
let make_data rules my_ticket tickets = { rules; my_ticket; tickets }

let parse input =
  let num = take_while Char.is_digit >>| Int.of_string in
  let ticket = sep_by (char ',') num in
  let rule_name = take_till (Char.equal ':') <* string ": " in
  let range = lift2 (fun x y -> (x, y)) (num <* char '-') num in
  let rule = lift3 make_rule rule_name (range <* string " or ") range in
  let rules = sep_by end_of_line rule in
  let my_ticket = string "your ticket:\n" *> ticket in
  let tickets = string "nearby tickets:\n" *> (sep_by end_of_line ticket) in
  let run = (skip_many (char '\n')) in
  let all = lift3 make_data (rules <* run) (my_ticket <* run) tickets in
  match parse_string all ~consume:All input with
  | Ok x -> x
  | Error x -> failwith x

let ( >|< ) x (l, u) = (x < l || u < x)

let is_valid rules ticket =
  List.rev_filter ticket ~f:(fun x -> List.for_all rules ~f:(fun r -> x >|< r.lo_range && x >|< r.hi_range))
  |> List.sum (module Int) ~f:ident

let ticket_translation input =
  let data = parse input in
  List.rev_map data.tickets ~f:(is_valid data.rules)
  |> List.sum (module Int) ~f:ident


let () =
  In_channel.create "./src/aoc16/input.txt"
  |> In_channel.input_all
  |> ticket_translation
  |> Format.printf "%d\n"