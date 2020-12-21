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

let rec solve acc candidates =
  if List.is_empty candidates then
    acc
  else
    let ingredient, allergen = List.find_map_exn candidates ~f:(fun (all, set) ->
      if Set.length set = 1 then Some (all, Set.max_elt_exn set) else None)
    in
    List.map candidates ~f:(Tuple2.map_snd ~f:(Fn.flip Set.remove allergen))
    |> List.filter ~f:(fun (_, set) -> not (Set.is_empty set))
    |> solve ((ingredient, allergen ):: acc)

let intersect foods allergen =
  allergen,
  List.filter_map foods ~f:(fun (ing, all) -> Option.some_if (Set.mem all allergen) ing)
  |> List.reduce_exn ~f:Set.inter

let allergen_assessment input =
  let foods = parse_input input in

  List.map ~f:Tuple2.get2 foods |> Set.union_list (module String) |> Set.to_list
  |> List.map ~f:(intersect foods)
  |> solve []
  |> List.sort ~compare:(fun (al1, _) (al2, _) -> String.compare al1 al2)
  |> List.map ~f:Tuple2.get2
  |> String.concat ~sep:","


let () =
  In_channel.create "./src/aoc21/input.txt"
  |> In_channel.input_all
  |> allergen_assessment
  |> Format.printf "%s\n"