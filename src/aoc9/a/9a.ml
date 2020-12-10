open Core
open Stdio

let encoding_error preamble input =
  let current = List.take input preamble |> Set.of_list (module Int) in
  let input = Array.of_list input in

  let rec loop cur i j =
    if j >= Array.length input then
      -1
    else if Set.exists cur ~f:(fun x -> Set.exists cur ~f:(fun y -> x + y = input.(j))) then
      let cur = Set.remove cur input.(i) in
      let cur = Set.add cur input.(j) in
      loop cur (i + 1) (j + 1)
    else
      input.(j)
  in
  loop current 0 preamble

let () =
  In_channel.create "./src/aoc9/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> encoding_error 25
  |> Stdio.printf "%d\n"
