open Core
open Stdio

let adapter_array input =
  let input = List.sort input ~compare:Int.compare in

  (*  .. x0, x1, x2, x3 ...xs*)
  let rec loop x0 x1 x2 s0 s1 s2 rest =
    match rest with
    | [] -> s2 + (if x2 +3 - x2 <= 3 && x2 + 3 - x1 <= 3 then s1 + (if x2+3 - x0 <= 3 then s0 else 0) else 0)
    | x3 :: xs ->
(*       printf "x(%d %d %d) %d\ns(%d %d %d)\n\n" x0 x1 x2 x3 s0 s1 s2; *)
      let scur = s2 + (if x3 - x2 <= 3 && x3 - x1 <= 3 then s1 + (if x3 - x0 <= 3 then s0 else 0) else 0) in
      loop x1 x2 x3 s1 s2 scur xs
  in
  match input with
  | x1 :: x2 :: xs -> begin
      let s0 = 1 in
      let s1 = 1 in
      let s2 = s1 + (if x1 < 3 && x2 < 3 then s0 else 0) in
      printf "%d %d %d\n" s0 s1 s2;
      loop 0 x1 x2 s0 s1 s2 xs
    end
  | _ -> assert false

let () =
  In_channel.create "./src/aoc10/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> adapter_array
  |> Stdio.printf "%d\n"
