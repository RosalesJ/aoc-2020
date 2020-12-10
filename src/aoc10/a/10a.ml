open Core
open Stdio

let adapter_array input =
  let input = List.sort input ~compare:Int.compare in

  let inc acc x x2 =
    match acc, x2 - x with
    | (a, b, c), 1 -> (a + 1, b, c)
    | (a, b, c), 2 -> (a, b + 1, c)
    | (a, b, c), 3 -> (a, b, c + 1)
    | _ -> assert false
  in

  let rec loop last acc rest =
    match acc, rest with
    | (a,b,c), [] -> (a, b, c + 1)
    | _, x :: xs -> loop x (inc acc last x) xs
  in
  match loop 0 (0, 0, 0) input with
  | (a, b, c) -> printf "%d, %d, %d\n" a b c; a * c

let () =
  In_channel.create "./src/aoc10/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> adapter_array
  |> Stdio.printf "%d\n"
