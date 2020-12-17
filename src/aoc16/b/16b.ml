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

let ( >< ) x (l, u) = (l <= x && x <= u)
let validate x r = x >< r.lo_range || x >< r.hi_range
let is_valid rules ticket = List.for_all ticket ~f:(fun x -> List.exists rules ~f:(validate x))

let find_rule rules i map arr =
  List.filter rules ~f:(fun r -> Array.for_all arr ~f:Fn.(flip validate r))
  |> List.fold ~init:map ~f:(fun acc x -> Map.add_multi acc ~key:x.name ~data:(i, x))

let rec decode acc matches =
  if List.is_empty matches then acc
  else
    let (field_index, _) as l = List.find_map_exn ~f:(fun rules ->
      if List.length rules = 1
      then Some (List.hd_exn rules)
      else None) matches
    in
    List.map matches ~f:(fun rules -> List.filter rules ~f:(fun (i, _) -> not (field_index = i)))
    |> List.filter ~f:(fun x -> not (0 = List.length x))
    |> decode (l :: acc)

let ticket_translation input =
  let data = parse input in
  let valid_tickets = List.filter data.tickets ~f:(is_valid data.rules) in
  let departure_indexes =
    data.my_ticket :: valid_tickets
    |> List.map ~f:(Array.of_list)
    |> Array.of_list
    |> Array.transpose_exn
    |> Array.foldi ~init:(Map.empty (module String)) ~f:(find_rule data.rules)
    |> Map.data
    |> decode []
    |> List.filter_map ~f:(fun (i, rule) -> Option.some_if
      (String.is_substring rule.name ~substring:"departure") i)
  in

  List.filteri data.my_ticket ~f:(fun i _ -> List.mem departure_indexes i ~equal:Int.equal)
  |> List.fold ~f:(( * )) ~init:1


let () =
  In_channel.create "./src/aoc16/input.txt"
  |> In_channel.input_all
  |> ticket_translation
  |> Format.printf "%d\n"