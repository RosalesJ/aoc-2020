open Core
open Stdio

let show arr =
  Array.iter ~f:(fun x -> Array.iter x ~f:(printf "%c "); print_endline "") arr; print_endline ""

let count_neighbors input i j c =
  let mx, my = Array.length (input.(0)), Array.length input in
  let around k = (Sequence.range (k - 1) (k + 1) ~start:`inclusive ~stop:`inclusive) in
  Sequence.count (Sequence.cartesian_product (around i) (around j))
    ~f:(fun (m, n) ->
      if m < 0 || m >= my || n < 0 || n >= mx || (i = m && j = n) then false
      else
        Char.equal c input.(m).(n))

let seating_system input =
  let input = List.map input ~f:String.to_array |> Array.of_list in

  let rec loop last_gen =
    let new_gen = Array.mapi last_gen ~f:(fun i -> Array.mapi ~f:(fun j x ->
      match x with
      | 'L' -> if count_neighbors last_gen i j '#' = 0 then '#' else 'L'
      | '#' -> if count_neighbors last_gen i j '#' >= 4 then 'L' else '#'
      | x -> x))
    in
    if Array.equal (Array.equal Char.equal) new_gen last_gen then
      Array.sum (module Int) new_gen ~f:(fun x -> Array.count ~f:(Char.equal '#') x)
    else
      loop new_gen
  in
  loop input

let () =
  In_channel.create "./src/aoc11/input.txt"
  |> In_channel.input_lines
  |> seating_system
  |> Stdio.printf "%d\n"