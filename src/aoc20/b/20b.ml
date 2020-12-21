open Core
open Angstrom

type tile = { id: int; body: char list list }
let make_tile id body = { id; body }

let fst = Fn.flip List.nth_exn 0
let rev = List.rev
let maprev = List.map ~f:rev
let tr = List.transpose_exn
let lst = List.last_exn

let show = List.iter ~f:(fun x -> printf "%s\n" (String.concat (List.map x ~f:(sprintf "%c"))))

let sides { body; _ } =
  let size = List.length body in
  let trans_body = List.transpose_exn body in
  let fst = Fn.flip List.nth_exn 0 in
  let lst = Fn.flip List.nth_exn (size - 1) in
  [fst body; lst body |> rev; fst trans_body |> rev; lst trans_body]
  |> List.map ~f:(List.to_list)
  |> List.map ~f:(String.of_char_list)

let padded body =
  let size = List.length body in
  let pad = List.init size ~f:(Fn.const ' ') in
  let s = List.slice body 0 size in
  let trans = List.transpose_exn s in
  let s = List.slice trans 0 size in
  (' ' :: pad) :: List.transpose_exn (pad :: s)

let interior body =
  let size = List.length body in
  let s = List.slice body 1 (size - 1) in
  let trans = List.transpose_exn s in
  let s = List.slice trans 1 (size - 1) in
  List.transpose_exn s

let lookup edge_map edge =
  match Map.find edge_map edge with
  | Some x -> x
  | None -> Map.find_exn edge_map (String.rev edge)

let edge_equal v w = String.equal v w || String.equal (String.rev v) w

let snap_down tile edge =
  let body = tile.body in
  let trans = List.transpose_exn body in

  let (=) a b = String.equal a (String.of_char_list b) in
  let (=~) a b = String.rev a = b in

  let oriented =
    match edge with
    | s when s = fst body   -> body
    | s when s = lst body   -> body |> rev
    | s when s =~ fst body  -> body |> maprev
    | s when s =~ lst body  -> body |> rev |> maprev
    | s when s = fst trans  -> trans
    | s when s = lst trans  -> trans |> rev
    | s when s =~ fst trans -> trans |> maprev
    | s when s =~ lst trans -> trans |> rev |> maprev
    | _ -> failwith "None of the edges matched!"
  in
  let next_left = lst oriented in
  oriented, (String.of_char_list next_left)

let find_next_left tile next_left_edge =
  let oriented_left, next_left_edge = snap_down tile next_left_edge in
  let rotated_back = oriented_left |> tr |> maprev in
  let next_down_edge = String.of_char_list (fst rotated_back) in
  next_down_edge, next_left_edge

let assemble tiles =
  let try_add map tile =
    let sides = sides tile in
    List.fold sides ~init:map ~f:(fun map side ->
      match Map.mem map side with
      | true -> Map.add_multi map ~key:side ~data:tile
      | false -> Map.add_multi map ~key:(String.rev side) ~data:tile)
  in
  let edge_map = List.fold tiles ~init:(Map.empty (module String)) ~f:try_add in

  let rec loop (finished: (int, 'a) Set_intf.Set.t) next_down_edge next_left_edge rows cur_row =
    printf "Begin iteration: (finished: %d) (rows: %d) (cur_row: %d) (next_left: %s) (next_down: %s)\n" (Set.length finished) (List.length rows) (List.length cur_row) next_left_edge next_down_edge;
    let down_tile =
      let down_candidates = lookup edge_map next_down_edge in
      printf "down_candidates: %s\n" (String.concat (List.map down_candidates ~f:(fun x -> sprintf "%d " x.id)));
      List.find down_candidates ~f:(fun x -> not (Set.mem finished x.id))
    in
    match down_tile with
    | Some down_tile ->
      printf "visiting %d\n" down_tile.id;
      let oriented_down_tile, next_down_edge = snap_down down_tile next_down_edge in
      loop (Set.add finished down_tile.id) next_down_edge next_left_edge rows (interior oriented_down_tile :: cur_row)
    | None ->
      printf "next row\n";
      let left_tile = lookup edge_map next_left_edge
        |> List.find ~f:(fun x -> not (Set.mem finished x.id))
      in
      match left_tile with
      | None -> cur_row :: rows
      | Some left_tile ->
        let next_down, next_left = find_next_left left_tile next_left_edge in
        loop finished next_down next_left (cur_row :: rows) []
  in
  (* let corner = List.find_exn tiles ~f:(fun tile -> 2 = (List.count (sides tile) ~f:(fun side -> 2 = List.length (lookup edge_map side)))) in *)
  (* let exterior_edges = List.filter (sides corner) ~f:(fun side -> 1 = List.length (lookup edge_map side)) in *)
  (* let next_down = String.rev "#...##.#.." in
  let next_left = ".#..#####." in *)

  let next_down = String.rev "#........." in
  let next_left = ".#..#.##.#" in
  (* let next_down = List.hd_exn exterior_edges in
  let next_left = List.find_exn (sides corner) ~f:(fun side -> 2 = List.length (lookup edge_map side))in *)

  let finished = Set.empty (module Int) in
  let open List in
  loop finished next_down next_left [] []
  |> List.transpose_exn
  |> List.rev
  >>| List.reduce_exn ~f:(fun acc x -> List.zip_exn acc x >>| Tuple2.uncurry List.append)
  |> List.join

let parse_input input =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  let id = string "Tile " *> num <* string ":\n"in
  let row = take_while (function '.' | '#' -> true | _ -> false) <* end_of_line >>| String.to_list in
  let piece = count 10 row  in
  let tile = lift2 make_tile id piece in
  match parse_string (sep_by end_of_line tile) input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let jurassic_jigsaw input =
  let tiles = parse_input input in
  let pic = assemble tiles in
  show (pic |> rev);
  List.map ~f:String.of_char_list (pic |> rev)
  |> String.concat
  |> String.count ~f:(Char.equal '#')


let () =
  In_channel.create "./src/aoc20/input.txt"
  |> In_channel.input_all
  |> jurassic_jigsaw
  |> Format.printf "%d\n"