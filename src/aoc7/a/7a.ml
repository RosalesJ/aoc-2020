open Core
open Stdio
open Angstrom

type bag = { color: string; modifier: string}
type rule = { bag: bag; children: (int * bag) list }

let make_bag num modifier color = (num, { modifier; color })
let make_rule modifier color children = { bag = {color; modifier}; children }

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
  let add_all rule acc xs = List.fold xs
    ~init:acc
    ~f:(fun acc (_, x) -> Map.add_multi acc ~key:(bag_key x) ~data:rule)
  in

  let parents_map = List.fold rules ~init:(Map.empty (module String))
    ~f:(fun acc rule ->
      match rule.children with
      | [] -> acc
      | xs -> add_all rule acc xs )
  in

  let rec loop explored cur =
    if Set.is_empty cur then
      explored
    else
      Set.to_list cur
      |> List.concat_map ~f:(fun x -> Map.find_multi parents_map x)
      |> List.map ~f:(fun x -> (bag_key x.bag))
      |> List.filter ~f:(fun x -> not (Set.mem explored x))
      |> Set.of_list (module String)
      |> loop (Set.union cur explored)
  in

  let has_shiny_gold x = List.exists x.children ~f:(fun (_, y) ->
    String.equal "gold" y.color &&
    String.equal "shiny" y.modifier)
  in

  List.filter rules ~f:has_shiny_gold
  |> List.map ~f:(fun x -> (bag_key x.bag))
  |> Set.of_list (module String)
  |> loop (Set.empty (module String))
  |> Set.length

let () =
  In_channel.create "./src/aoc7/input.txt"
  |> In_channel.input_all
  |> handy_haversacks
  |> Stdio.printf "%d\n"