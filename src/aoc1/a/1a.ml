open Base
open Stdio

let report_repair target arr =
  Array.sort ~compare:Int.compare arr;

  let rec loop arr i j =
    match arr.(i) + arr.(j) with
    | _ when i = j      -> -1
    | x when x = target -> arr.(i) * arr.(j)
    | x when x > target -> loop arr i (j - 1)
    | x when x < target -> loop arr (i + 1) j
    | _                 -> -1
  in
  loop arr 0 (Array.length arr - 1)

let () =
  let target = 2020 in

  In_channel.create "./src/aoc1/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> Array.of_list
  |> report_repair target
  |> Stdio.printf "%d\n"