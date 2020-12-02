open Base
open Stdio

let report_repair target arr k =
  let rec loop arr i j =
    match arr.(i) + arr.(j) + arr.(k) with
    | _ when i = j      -> None
    | x when x = target -> Some (arr.(i) * arr.(j) * arr.(k))
    | x when x > target -> loop arr i (j - 1)
    | x when x < target -> loop arr (i + 1) j
    | _                 -> None
  in
  loop arr (k + 1) (Array.length arr - 1)

let report_repair3 target xs =
  Array.sort ~compare:Int.compare xs;

  let rec loop i =
    if i >= Array.length xs - 4 then -1
    else match report_repair target xs i with
    | None -> loop (i + 1)
    | Some x -> x
  in
  loop 0
      

let () =
  let target = 2020 in

  In_channel.create "./src/aoc1/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> Array.of_list
  |> report_repair3 target
  |> Stdio.printf "%d\n"