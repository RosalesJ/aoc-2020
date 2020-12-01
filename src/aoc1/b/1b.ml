open Base
open Stdio

let report_repair xs i =
  let target = 2020 in
  
  let rec loop arr i j k =
    Stdio.printf "%d, %d, %d:   %d\n" i j k (arr.(i) + arr.(j) + arr.(k));
    match arr.(i) + arr.(j) + arr.(k) with
    | _ when j = k      -> -1
    | x when x = target -> arr.(i) * arr.(j) * arr.(k)
    | x when x > target -> loop arr i j (k - 1)
    | x when x < target -> loop arr i (j + 1) k
    | _                 -> -1
  in
  loop xs i (i + 1) (Array.length xs - 1)

let report_repair3 xs =
  Array.sort ~compare:Int.compare xs;

  let rec loop i =
    if i >= Array.length xs - 4 then -1
    else 
      let value = report_repair xs i in
      if value <> -1 then
        value
      else
        loop (i + 1)
  in
  loop 0
      

let () =
  In_channel.create "./src/aoc1/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> Array.of_list
  |> report_repair3
  |> Stdio.printf "%d\n"