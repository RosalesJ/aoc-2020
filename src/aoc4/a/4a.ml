open Core
open Stdio

let birth_year = "byr"
let issue_year = "iyr"
let expiration_year = "eyr"
let height = "hgt"
let hair_color = "hcl"
let eye_color = "ecl"
let passport_id = "pid"
let country_id = "cid" (* excluded from fields to check*)

let passport_valid pp =
  let fields = [birth_year; issue_year; expiration_year; height; hair_color;
    eye_color; passport_id]
  in
  let f acc field = acc && Option.is_some (String.substr_index pp ~pattern:field) in
  List.fold ~f fields ~init:true
let passport_processing input =
  let xs = Str.split_delim (Str.regexp "\n\n") input in
  let results = List.map xs ~f:passport_valid in
  (* List.iter ~f:(printf "%b\n") results; *)
  List.count ~f:(fun x -> x) results


let () =
  In_channel.create "./src/aoc4/input.txt"
  |> In_channel.input_all
  |> passport_processing
  |> Stdio.printf "%d\n"