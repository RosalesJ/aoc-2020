open Core
open Stdio

let toboggan_trajectory matrix dx dy =
  let rec loop x y count =
    if y >= Array.length matrix then
      count
    else begin
      let cur_row = Array.get matrix y in
      let cur_space = String.get cur_row x in
      let x = (x + dx) mod (String.length cur_row) in
      let y = (y + dy) in
      if Char.equal '#' cur_space then
        loop x y (count + 1)
      else
        loop x y count
    end
  in
  loop 0 0 0

let all_trajectories trajectories input =
  let matrix = Array.of_list input in
  trajectories
  |> List.map ~f:(Tuple.T2.uncurry (toboggan_trajectory matrix))
  |> List.fold ~f:( * ) ~init:1

let () =
  In_channel.create "./src/aoc3/input.txt"
  |> In_channel.input_lines
  |> all_trajectories [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
  |> Stdio.printf "%d\n"