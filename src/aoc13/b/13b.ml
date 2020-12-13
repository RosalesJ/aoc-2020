open Core
open Stdio

let rec find_time init p inc off k =
  if (inc * k + init + off) mod p = 0 then
    inc * k + init
  else
    find_time init p inc off (k + 1)

let shuttle_search input =
  String.split ~on:',' (List.nth_exn input 1)
  |> List.filter_mapi ~f:(fun i x -> try Some (i, Int.of_string x) with Failure _ -> None)
  |> List.fold ~init:(1, 1) ~f:(fun (inc, init) (off, p) -> (inc * p, find_time init p inc off 0))
  |> Tuple.T2.get2

let () =
  In_channel.create "./src/aoc13/input.txt"
  |> In_channel.input_lines
  |> shuttle_search
  |> Stdio.printf "%d\n"