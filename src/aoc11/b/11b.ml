open Core
open Stdio

let show arr =
  Array.iter ~f:(fun x -> Array.iter x ~f:(printf "%c "); print_endline "") arr; print_endline ""

let occupied = '#'
let unoccupied = 'L'
let floor = '.'

let rec look input y x dy dx print =
  let mx, my = Array.length (input.(0)), Array.length input in
  let y, x = y + dy, x + dx in
  if y < 0 || y >= my ||x < 0 || x >= mx then
    false
  else
    match input.(y).(x) with
    | '#' -> true
    | 'L' -> false
    | '.' | _ -> look input  y x dy dx print

let count_neighbors input i j =
  let around k = (Sequence.range (k - 1) (k + 1) ~start:`inclusive ~stop:`inclusive) in
  let directions = Sequence.cartesian_product (around 0) (around 0) in
  let print = i = 2 && j = 2 in
  Sequence.count directions ~f:(function (0, 0) -> false |  (dy, dx) -> look input i j dy dx print)

let seating_system input =
  let input = List.map input ~f:String.to_array |> Array.of_list in

  let rec loop last_gen =
    let new_gen = Array.mapi last_gen ~f:(fun i -> Array.mapi ~f:(fun j x ->
      match x with
      | 'L' -> if count_neighbors last_gen i j = 0 then occupied else unoccupied
      | '#' -> if count_neighbors last_gen i j >= 5 then unoccupied else occupied
      | x -> x))
    in
    if Array.equal (Array.equal Char.equal) new_gen last_gen then
      Array.sum (module Int) new_gen ~f:(fun x -> Array.count ~f:(Char.equal occupied) x)
    else
      loop new_gen
  in
  loop input

let () =
  In_channel.create "./src/aoc11/input.txt"
  |> In_channel.input_lines
  |> seating_system
  |> Stdio.printf "%d\n"