open Core
open Stdio

let shuttle_search input =
  let timestamp = Int.of_string (List.hd_exn input) in
  let shuttles =
    String.split ~on:',' (List.nth_exn input 1)
    |> List.filter ~f:(Fn.non (String.equal "x"))
    |> List.map ~f:Int.of_string
  in

  let sched = List.map shuttles ~f:(fun x -> x, timestamp mod x - x) in

  List.max_elt ~compare:(fun (_, x) (_, y) -> Int.compare x y) sched
  |> function None -> -1 | Some (shuttle, x) -> -x * shuttle

let () =
  In_channel.create "./src/aoc13/input.txt"
  |> In_channel.input_lines
  |> shuttle_search
  |> Stdio.printf "%d\n"