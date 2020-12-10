open Core
open Stdio

let find_error preamble input =
  let current = List.take input preamble |> Set.of_list (module Int) in
  let input = Array.of_list input in

  let rec loop cur i j =
    if j >= Array.length input then
      -1
    else if Set.exists cur ~f:(fun x -> Set.exists cur ~f:(fun y -> x + y = input.(j))) then
      let cur = Set.remove cur input.(i) in
      let cur = Set.add cur input.(j) in
      loop cur (i + 1) (j + 1)
    else
      input.(j)
  in
  loop current 0 preamble

let encoding_error preamble input =
  let error = find_error preamble input in
  let input = Array.of_list input in

  let rec loop sum i j =
    match sum with
    | x when x = error -> (i, j)
    | x when x < error -> loop (x + input.(j + 1)) i (j + 1)
    | x                -> loop (x - input.(i)) (i + 1) j
  in
  let i, j = loop (input.(0) + input.(1)) 0 1 in
  let arr = Array.sub input ~pos:i ~len:(j - i) in
  let lis = Array.to_list arr in
  let open Option.Let_syntax in
  (List.min_elt ~compare:Int.compare lis >>= fun a ->
  List.max_elt ~compare:Int.compare lis >>= fun b ->
  return (a + b))
  |> function None -> -1 | Some x -> x

let () =
  In_channel.create "./src/aoc9/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> encoding_error 25
  |> Stdio.printf "%d\n"
