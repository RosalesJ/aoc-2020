open Core
open Angstrom

type tile = { x: int; y: int; z: int }

let reference_tile = { x = 0; y = 0; z = 0 }

let sexp_of_tile {x; y; z} = Tuple3.sexp_of_t Int.sexp_of_t Int.sexp_of_t Int.sexp_of_t (x, y, z)

let parse_input input =
  let e = string "e" *> return `E in
  let w = string "w" *> return `W in
  let ne = string "ne" *> return `NE in
  let nw = string "nw" *> return `NW in
  let se = string "se" *> return `SE in
  let sw = string "sw" *> return `SW in
  let dir = choice [ne; nw; se; sw; e; w] in
  let tiles = sep_by end_of_line (many dir) in
  match parse_string tiles input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let find_tile dirs =
  let rec loop ({x; y; z} as tile) dirs =
    match dirs with
    | [] -> tile
    | dir :: xs ->
      match dir with
      | `E -> loop  { tile with x = x + 1; y = y - 1 } xs
      | `W -> loop  { tile with x = x - 1; y = y + 1 } xs
      | `NE -> loop { tile with x = x + 1; z = z - 1 } xs
      | `SW -> loop { tile with x = x - 1; z = z + 1 } xs
      | `NW -> loop { tile with y = y + 1; z = z - 1 } xs
      | `SE -> loop { tile with y = y - 1; z = z + 1 } xs
  in
  loop reference_tile dirs

let lobby_layout input =
  let directions = parse_input input in
  let tiles = List.map directions ~f:find_tile in
  let ocurr_map = List.fold tiles ~init:(Map.empty (module Sexp)) ~f:(fun acc x -> Map.add_multi acc ~key:(sexp_of_tile x) ~data:x) in
  Map.to_alist ocurr_map
  |> List.map ~f:Tuple2.get2
  |> List.map ~f:List.length
  |> List.count ~f:(fun x -> x mod 2 = 1)


let () =
  In_channel.create "./src/aoc24/input.txt"
  |> In_channel.input_all
  |> lobby_layout
  |> Format.printf "%d\n"