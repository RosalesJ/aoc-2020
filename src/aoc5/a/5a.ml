open Core
open Stdio

type seat = { row: int; column: int }

type row = Subrow of { left: row; right: row } | Seat of seat

type plane = Section of { front: plane; back: plane } | Row of row

let show_seat { row; column } = sprintf "(%d,%d)" row column
let seat_id { row; column } = row * 8 + column

let split xs = List.groupi xs ~break:(fun i _ _ -> i = List.length xs / 2)

let rec show_row row =
  match row with
  | Seat s -> show_seat s
  | Subrow {left; right} -> sprintf "%s %s" (show_row left) (show_row right)

let rec show plane =
  match plane with
  | Row r -> show_row r
  | Section {front; back} -> sprintf "%s\n%s" (show front) (show back)

let rec seats_to_row seats =
  match split seats with
  | [[seat]]      -> Seat seat
  | [left; right] -> Subrow { left = seats_to_row left; right = seats_to_row right }
  | _             -> failwith "Couldn't make seats into rows"

let rec rows_to_section rows =
  match split rows with
  | [[row]]       -> Row row
  | [front; back] -> Section { front = rows_to_section front; back = rows_to_section back }
  | _             -> failwith "Couldn't make rows into sections"

let build_plane ~rows ~cols =
  let seats = List.init rows ~f:(fun row -> List.init cols ~f:(fun column -> { row; column })) in
  let rows = List.map seats ~f:seats_to_row in
  let plane = rows_to_section rows in
  plane

let decode_seat plane encoding =
  let rec get_row section i =
    if i > 7 then failwith (sprintf "row instruction out of bounds: '%d'" i);
    match String.get encoding i, section with
    | 'B', Section { back; _ }  -> get_row back (i + 1)
    | 'F', Section { front; _ } -> get_row front (i + 1)
    | _,   Row row when i = 7   -> row
    | x, _                         -> failwith (sprintf "Failed getting row at %c, %d" x i)
  in
  let rec get_seat row i =
    if i <= 6 || 10 < i then failwith (sprintf "seat instruction out of bounds: '%d'" i);
    match row with
    | Seat st when i = 10  -> st
    | Seat _ -> failwith "Found a seat too early!"
    | Subrow { left; right } ->
      match String.get encoding i with
      | 'L' -> get_seat left (i + 1)
      | 'R' -> get_seat right (i + 1)
      | x -> failwith (sprintf "Bad seat instruction: '%c'" x)
  in
  let row = get_row plane 0 in
  let seat = get_seat row 7 in
  seat

let binary_boarding seat_codings =
  let plane = build_plane ~cols:8 ~rows:128 in
  List.map ~f:(decode_seat plane) seat_codings
  |> List.map ~f:seat_id
  |> List.max_elt ~compare:Int.compare
  |> function None -> 0 | Some x -> x

let () =
try
  (* build_plane ~cols:8 ~rows:128
  |> show
  |> printf "%s\n"; *)
  In_channel.create "./src/aoc5/input.txt"
  |> In_channel.input_lines
  |> binary_boarding
  |> Stdio.printf "%d\n"
with Failure f -> printf "I'm dumb: %s" f
