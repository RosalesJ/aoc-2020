open Core
open Stdio

let collect_responses = String.fold ~f:Set.add ~init:(Set.empty (module Char))

let tally_group group =
  let responses =
    let people = String.split_lines group in
    List.map people ~f:collect_responses
  in
  List.fold responses ~init:(List.hd_exn responses) ~f:Set.inter
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
