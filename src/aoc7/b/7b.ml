open Core
open Stdio
open Angstrom

type bag = { color: string; modifier: string}
type rule = { bag: bag; children: (int * bag) list }

let make_bag num modifier color = (num, { modifier; color })
let make_rule modifier color children = { bag = {color; modifier}; children }

let show_bag {color; modifier} = sprintf "%s %s bag" color modifier
let show_rule {bag; children} =
  sprintf "%s bags contain %s."
    (show_bag bag)
    (match children with
    | [] -> "nothing lol"
    | c -> (List.map c ~f:(fun (num, bag) -> sprintf "%d %s" num (show_bag bag)) |> String.concat ~sep:", "))

let rule =
  let spc = char ' ' in
  let bagend = (string "bags" <|> string "bag") <* (char '.' <|> char ',') in
  let num = take_while Char.is_digit <* spc >>| fun x -> Int.of_string x in
  let modifier = take_while Char.is_alpha <* spc in
  let color = take_while Char.is_alpha <* spc in
  let child = spc *> lift3 make_bag num modifier (color <* bagend) in
  let children = string " no other bags." *> return [] <|> many child in

  lift3 make_rule modifier (color <* string "bags contain") children

let parse_rules input =
  let parser = (sep_by (char '\n') rule) in
  match parse_string parser ~consume:All input with
  | Ok x -> x
  | Error e -> failwith e

let handy_haversacks input =
  let bag_key { modifier; color } = modifier ^ color in
  let rules = parse_rules input in

  let forward_map = List.fold rules ~init:(Map.empty (module String)) ~f:(fun acc ({bag; _} as rule) -> Map.add_exn acc ~key:(bag_key bag) ~data:rule) in

  let gold_bag = List.find_exn rules ~f:(fun {bag={color; modifier}; _} -> String.equal "gold" color && String.equal "shiny" modifier) in


  let rec loop rule = rule |> fun { children; _ } ->
    List.map children ~f:(fun (multiplier, bag) ->
      let next_rule = Map.find_exn forward_map (bag_key bag) in
      multiplier * loop next_rule)
    |> List.sum (module Int) ~f:Fn.id
    |> fun x -> x + 1
  in

  loop gold_bag - 1

let () =
  In_channel.create "./src/aoc7/input.txt"
  |> In_channel.input_all
  |> handy_haversacks
  |> Stdio.printf "%d\n"
