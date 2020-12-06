open Core
open Stdio

let tally_group group =
  let empty = Set.empty (module Char) in
  String.split_lines group
  |> List.map ~f:(String.fold ~init:empty ~f:Set.add)
  |> List.fold ~init:empty ~f:Set.union
  |> Set.count ~f:(Fn.const true)

let custom_customs input =
  Str.split_delim (Str.regexp "\n\n") input
  |> List.map ~f:tally_group
  |> List.sum (module Int) ~f:Fn.id

let () =
  In_channel.create "./src/aoc6/input.txt"
  |> In_channel.input_all
  |> custom_customs
  |> Stdio.printf "%d\n"
