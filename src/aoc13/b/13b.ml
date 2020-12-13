open Core
open Stdio

let shuttle_search input =
  let rec closest_time (step, time) (c, p) =
    if (time + c) mod p = 0 then
      (step * p, time)
    else
      closest_time (step, time + step) (c, p)
  in

  String.split ~on:',' (List.nth_exn input 1)
  |> List.filter_mapi ~f:(fun i x -> try Some (i, Int.of_string x) with Failure _ -> None)
  |> List.fold ~init:(1, 1) ~f:closest_time
  |> Tuple2.get2

let () =
  In_channel.create "./src/aoc13/input.txt"
  |> In_channel.input_lines
  |> shuttle_search
  |> Stdio.printf "%d\n"