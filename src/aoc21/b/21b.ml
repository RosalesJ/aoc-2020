open Core
open Angstrom

let parse_input input =
  let str = take_while Char.is_alpha in
  let ingredients = many1 (str <* char ' ') >>| Set.of_list (module String) in
  let allergens = string "(contains " *> (sep_by (string ", ") str) <* char ')' >>| Set.of_list (module String) in
  let food = lift2 Tuple2.create ingredients allergens in
  match parse_string (sep_by end_of_line food) input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let allergen_assessment input =
  let foods = parse_input input in
  let _all_ingredients = List.map ~f:Tuple2.get1 foods |> List.map ~f:Set.to_list |> List.join in
  let all_allergens = List.map ~f:Tuple2.get2 foods |> Set.union_list (module String) |> Set.to_list in
  let allergen_common_ingredients =
    List.map all_allergens ~f:(fun allergen ->
      allergen,
      List.filter_map foods ~f:(fun (ing, all) ->
        Option.some_if (Set.mem all allergen) ing)
      |> List.reduce_exn ~f:Set.inter)
  in

  let rec loop acc map =
    if List.is_empty map then
      acc
    else
      let ingredient, allergen = List.find_map_exn map ~f:(fun (all, set) -> if Set.length set = 1 then Some (all, Set.max_elt_exn set) else None) in
      let map = List.map ~f:(Tuple2.map_snd ~f:(Fn.flip Set.remove allergen)) map in
      let map = List.filter ~f:(fun (_, set) -> not (Set.is_empty set)) map in
      loop ((ingredient, allergen ):: acc) map
  in
  let results = loop [] allergen_common_ingredients in
  let results = List.sort ~compare:(fun (al1, _) (al2, _) -> String.compare al1 al2) results in
  List.iter ~f:(fun (ing, al) -> printf "%s: %s\n" ing al) results;
  String.concat ~sep:"," (List.map ~f:Tuple2.get2 results)


let () =
  In_channel.create "./src/aoc21/input.txt"
  |> In_channel.input_all
  |> allergen_assessment
  |> Format.printf "%s\n"