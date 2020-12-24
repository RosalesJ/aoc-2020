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

let apply_move ({ x; y; z } as tile) = function
  | `E -> { tile with x = x + 1; y = y - 1 }
  | `W ->  { tile with x = x - 1; y = y + 1 }
  | `NE -> { tile with x = x + 1; z = z - 1 }
  | `SW -> { tile with x = x - 1; z = z + 1 }
  | `NW -> { tile with y = y + 1; z = z - 1 }
  | `SE -> { tile with y = y - 1; z = z + 1 }

let find_tile dirs =
  let rec loop tile = function
    | [] -> tile
    | dir :: xs -> loop (apply_move tile dir) xs
  in
  loop reference_tile dirs

let range = (List.range (-1) 1 ~start:`inclusive ~stop:`inclusive)
let deltas =
  List.cartesian_product range (List.cartesian_product range range)
  |> List.filter_map ~f:(fun (dx, (dy, dz)) ->
     Option.some_if (not (dx + dy + dz = 0)) (dx, dy, dz))

let neighbors tile =
  [ `E; `W; `NE; `SW; `NW; `SE]
  |> List.map ~f:(apply_move tile)

let count_neighbors tile_map tile =
  List.count (neighbors tile) ~f:(fun n ->
    match Map.find tile_map (sexp_of_tile n) with
    | None -> false
    | Some (_, flipped) ->
      match flipped with
      | `Black -> true
      | `White -> false)

let flip = function `Black -> `White | `White -> `Black

let lobby_layout _days input =
  let directions = parse_input input in
  let tiles = List.map directions ~f:find_tile in
  let ocurr_map = List.fold tiles ~init:(Map.empty (module Sexp)) ~f:(fun acc x ->
    Map.update acc (sexp_of_tile x) ~f:(function None -> (x, `Black) | Some (x, flipped) -> (x, flip flipped)))
  in

  let rec loop i occur_map =
    printf "Day %d: %d\n" i (Map.count occur_map ~f:(fun (_, flipped) -> phys_equal `Black flipped));
    Stdlib.flush Stdio.stdout;
    if i = _days then
      occur_map
    else
      let data = Map.data occur_map in
      let all_tiles =
        List.concat_map data ~f:(fun (x, _) -> x :: neighbors x)
        |> List.dedup_and_sort ~compare:(fun x y -> Sexp.compare (sexp_of_tile x) (sexp_of_tile y))
      in
      let flipped_map = List.map all_tiles ~f:(fun tile ->
        match Map.find occur_map (sexp_of_tile tile), count_neighbors occur_map tile with
        | Some (_, `White), 2 -> tile, `Black
        | None,             2 -> tile, `Black
        | Some (_, `Black), x when x > 2 || x = 0 -> tile, `White
        | None, _ -> tile, `White
        | Some (_, col), _ -> tile, col)
      in
      let alist = List.filter_map flipped_map ~f:(fun (t, fl) -> Option.some_if (phys_equal `Black fl) (sexp_of_tile t, (t, fl))) in
      loop (i + 1) (Map.of_alist_exn (module Sexp) alist)
  in

  loop 0 ocurr_map
  |> Map.count ~f:(fun (_, flipped) -> phys_equal `Black flipped)


let () =
  In_channel.create "./src/aoc24/input.txt"
  |> In_channel.input_all
  |> lobby_layout 100
  |> Format.printf "%d\n"