open Core
open Stdio

let adapter_array input =
  let input = List.sort input ~compare:Int.compare in

  let rec loop acc last rest =
    match rest with
    | _ :: [] | [] -> acc + 1
    | x :: (x1 :: _ as xs) when x1 - last <= 3 -> loop acc x xs + loop acc last xs
    | x :: xs -> loop acc x xs
  in
  loop 0 0 input

let () =
  In_channel.create "./src/aoc10/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> adapter_array
  |> Stdio.printf "%d\n"
