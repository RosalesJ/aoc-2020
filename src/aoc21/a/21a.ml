open Core
open Angstrom

let parse_input input =
  let str = take_while Char.is_alpha in
  let ingredients = many1 (str <* char ' ') in
  let allergens = string "(contains " *> (sep_by (string ", ") str) <* char ')' in
  let food = lift2 Tuple2.create ingredients allergens in
  match parse_string (sep_by end_of_line food) input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let allergen_assessment input =
  let foods = parse_input input in
  let all_ingredients = List.map ~f:Tuple2.get1 foods |> List.join in
  let all_allergens = List.map ~f:Tuple2.get2 foods |> List.join in
  let possible =
    List.map all_allergens ~f:(fun allergen ->
      List.filter_map foods ~f:(fun (ing, all) ->
        Option.some_if (List.mem all allergen ~equal:String.equal) ing)
      |> List.map ~f:(Set.of_list (module String))
      |> List.reduce_exn ~f:Set.inter)
    |> List.reduce_exn ~f:Set.union
  in
  List.count all_ingredients ~f:Fn.(non (Set.mem possible))


let () =
  In_channel.create "./src/aoc21/input.txt"
  |> In_channel.input_all
  |> allergen_assessment
  |> Format.printf "%d\n"