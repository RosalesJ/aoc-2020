open Core
open Stdio

let shuttle_search input =
  let timestamp = Int.of_string (List.hd_exn input) in

  String.split ~on:',' (List.nth_exn input 1)
  |> List.filter ~f:(Fn.non (String.equal "x"))
  |> List.map ~f:Int.of_string
  |> List.map ~f:(fun x -> x, timestamp mod x - x)
  |> List.max_elt ~compare:(fun (_, x) (_, y) -> x - y)
  |> function None -> -1 | Some (shuttle, x) -> - x * shuttle

let () =
  In_channel.create "./src/aoc13/input.txt"
  |> In_channel.input_lines
  |> shuttle_search
  |> Stdio.printf "%d\n"