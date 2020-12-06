open Core
open Stdio

let blue = Set.inter
let empty = Set.empty (module Char)

let collect_responses = String.fold ~init:empty ~f:Set.add

let tally_group group =
  let r = String.split_lines group
  |> List.map ~f:collect_responses
  in
  List.fold r ~init:(List.hd_exn r) ~f:Set.inter
  |> Set.count ~f:(Fn.const true)

let custom_customs input =
  let xs = Str.split_delim (Str.regexp "\n\n") input in
  List.map xs ~f:tally_group
  |> List.sum (module Int) ~f:Fn.id

let () =
  In_channel.create "./src/aoc6/input.txt"
  |> In_channel.input_all
  |> custom_customs
  |> Stdio.printf "%d\n"
