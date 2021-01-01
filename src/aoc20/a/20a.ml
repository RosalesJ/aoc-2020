open Core
open Angstrom

type tile = { id: int; body: char array array }
let make_tile id body = { id; body }

let sides { body; _ } =
  let size = Array.length body in
  let trans_body = Array.transpose_exn body in
  [body.(0); body.(size - 1); trans_body.(0); trans_body.(size - 1)]
  |> List.map ~f:(Array.to_list)
  |> List.map ~f:(String.of_char_list)

let parse_input input =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  let id = string "Tile " *> num <* string ":\n"in
  let row = take_while (function '.' | '#' -> true | _ -> false) <* end_of_line >>| String.to_array in
  let piece = count 10 row >>| List.to_array in
  let tile = lift2 make_tile id piece in
  match parse_string (sep_by end_of_line tile) input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let jurassic_jigsaw input =
  let tiles = parse_input input in
  let sides = List.map tiles ~f:(fun x -> x.id, sides x) in
  let try_add map (id, sides) =
    List.fold sides ~init:map ~f:(fun map side ->
      match Map.mem map side with
      | true -> Map.add_multi map ~key:side ~data:id
      | false -> Map.add_multi map ~key:(String.rev side) ~data:id)
  in
  let side_map = List.fold sides ~init:(Map.empty (module String)) ~f:try_add in

  let edges =
    Map.to_alist side_map
    |> List.filter_map ~f:(fun (e, x) -> Option.some_if (List.length x = 1) (List.hd_exn x, e))
    |> Map.of_alist_multi (module Int)
  in
  Map.filter edges ~f:(fun x -> List.length x = 2)
  |> Map.to_alist
  |> List.fold ~init:1 ~f:(fun acc (id, _) -> id * acc)


let () =
  In_channel.create "./src/aoc20/input.txt"
  |> In_channel.input_all
  |> jurassic_jigsaw
  |> Format.printf "%d\n"