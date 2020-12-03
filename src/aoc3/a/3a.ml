open Core
open Stdio

let toboggan_trajectory dx rows =
  let tree = '#' in
  let f (pos, count) row =
    let current_space = (String.get row pos) in
    let pos = (pos + dx) mod (String.length row) in
    if Char.equal tree current_space then
      (pos, count + 1)
    else
      (pos, count)
  in
  List.fold ~f rows ~init:(0, 0)
  |> Tuple.T2.get2

let () =
  In_channel.create "./src/aoc3/input.txt"
  |> In_channel.input_lines
  |> toboggan_trajectory 3
  |> Stdio.printf "%d\n"