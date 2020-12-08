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
  let add_all rule acc xs = List.fold xs ~init:acc ~f:(fun acc (_, x) -> Map.add_multi acc ~key:(bag_key x) ~data:rule) in

  let contain_map = List.fold rules ~init:(Map.empty (module String)) ~f:(fun acc ({children; _} as rule) ->
    match children with
    | [] -> acc
    | xs -> add_all rule acc xs )
  in

  let has_shiny_gold {children; _} = List.exists children ~f:(fun (_, {color; modifier}) -> String.equal "gold" color && String.equal "shiny" modifier) in

  let have_gold = List.filter rules ~f:has_shiny_gold in
  let explored = Set.empty (module String) in

  (* List.iter have_gold ~f:(fun x -> printf "%s\n" (show_rule x)); *)

  let rec loop cur bags explored =
    if List.is_empty cur then
      bags
    else
      let explored = List.fold cur ~init:explored ~f:(fun acc x -> Set.add acc (bag_key x.bag)) in
      let parent_rules = List.concat_map cur ~f:(fun x -> match Map.find contain_map (bag_key x.bag) with None -> [] | Some xs -> xs) in
      let to_explore = List.stable_dedup (List.filter parent_rules ~f:(fun x -> not (Set.mem explored (bag_key x.bag)))) in
      (* printf "%d\n" (Set.length explored); *)
      loop to_explore (List.rev_append bags cur) explored
  in
  let results = loop have_gold [] explored in
  List.iter ~f:(fun x -> printf "%s\n" (show_rule x)) results;

  List.map results ~f:(fun x -> x.bag.color)
  |> List.stable_dedup
  |> List.length
  |> printf "%d\n";

  List.map results ~f:(fun x -> bag_key x.bag)
  |> List.stable_dedup
  |> List.length

let () =
  In_channel.create "./src/aoc7/input.txt"
  |> In_channel.input_all
  |> handy_haversacks
  |> Stdio.printf "%d\n"
