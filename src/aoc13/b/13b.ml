open Core
open Stdio

let rec find_coeff init p inc off k =
  (* printf "%d + (%d * %d) + %d mod %d = %d\n" init inc k off p ((init + (inc * k) + off) mod p); *)
  if ((inc * k) + init + off) mod p = 0 then
    (inc * k) + init
  else
    find_coeff init p inc off (k + 1)

let shuttle_search input =
  let shuttles =
    String.split ~on:',' (List.nth_exn input 1)
    |> List.filter_mapi ~f:(fun i x ->
      try Some (i, Int.of_string x)
    with Failure _ -> None)
  in
  let init = List.hd_exn shuttles |> Tuple.T2.get2 |> fun x -> (x, x) in
  List.fold ~init:init (List.tl_exn shuttles) ~f:(fun (inc, init) (off, p) ->
    let found = find_coeff init p inc off 0 in
    let inc = inc * p in
    (inc, found)
  )
  |> Tuple.T2.get2

let () =
  In_channel.create "./src/aoc13/input.txt"
  |> In_channel.input_lines
  |> shuttle_search
  |> Stdio.printf "%d\n"