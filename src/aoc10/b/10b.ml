open Core
open Stdio

let adapter_array input =
  let input = List.sort input ~compare:Int.compare in
  let sub_guard a b c = if a - b <= 3 then c else 0 in

  let rec loop x0 x1 x2 s0 s1 s2 rest =
    match rest with
    | [] -> s2
    | x3 :: xs ->
      let scur = s2 + sub_guard x3 x1 (s1 + sub_guard x3 x0 s0) in
      loop x1 x2 x3 s1 s2 scur xs
  in
  match input with
  | x1 :: x2 :: xs -> (
      let s0 = 1 in
      let s1 = 1 in
      let s2 = s1 + sub_guard x1 0 1 in
      loop 0 x1 x2 s0 s1 s2 xs)
  | _ -> assert false

let () =
  In_channel.create "./src/aoc10/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> adapter_array
  |> Stdio.printf "%d\n"
