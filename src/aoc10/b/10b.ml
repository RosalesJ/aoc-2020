open Core
open Stdio

let adapter_array input =
  let input = List.sort input ~compare:Int.compare in
  let test a b c = if a - b <=3 then c else 0 in

  let rec loop x0 x1 x2 s0 s1 s2 rest =
    match rest with
    | [] -> s2
    | x3 :: xs ->
      let scur = s2 + test x3 x1 (s1 + test x3 x0 s0) in
      loop x1 x2 x3 s1 s2 scur xs
  in
  match input with
  | x1 :: x2 :: xs -> begin
      let s0 = 1 in
      let s1 = 1 in
      let s2 = s1 + test x1 0 1 in
      loop 0 x1 x2 s0 s1 s2 xs
    end
  | _ -> assert false

let () =
  In_channel.create "./src/aoc10/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> adapter_array
  |> Stdio.printf "%d\n"
