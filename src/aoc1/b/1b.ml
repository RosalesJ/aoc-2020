open Base
open Stdio

let report_repair xs =
  let target = 2020 in
  let arr = Array.of_list xs in
  Array.sort ~compare:Int.compare arr;

  let rec loop arr i j =
    match arr.(i) + arr.(j) with
    | x when x = target -> arr.(i) * arr.(j)
    | x when x > target -> loop arr i (j - 1)
    | x when x < target -> loop arr (i + 1) j
    | _ when i = j      -> -1
    | _                 -> -1
  in
  loop arr 0 (Array.length arr - 1)

let () =
  In_channel.create "./src/aoc1/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> report_repair
  |> Stdio.printf "%d\n"