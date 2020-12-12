open Core
open Stdio
open Angstrom

type instr = { typ: [`N | `S | `E | `W | `L | `R | `F]; num:int }
type ship = { pos: Complex.t; dir: Complex.t }

let make_instr typ num = { typ; num }
let starting_ship = { pos = Complex.zero; dir = Complex.one }
let taxicab { pos; _ } = abs (Int.of_float pos.re) + abs (Int.of_float pos.im)

let parse_instr input =
  let typ = any_char >>| function
    | 'N' -> `N | 'S' -> `S | 'E' -> `E | 'W' -> `W | 'L' -> `L
    | 'R' -> `R | 'F' -> `F | x -> failwith (sprintf "Bad intruction %c" x)
  in
  let num = take_while Char.is_digit >>| Int.of_string in
  let parser = lift2 make_instr typ num in
  match parse_string (sep_by (char '\n') parser) input ~consume:All with
  | Ok instrs -> instrs
  | Error x -> printf "%s\n" x; []

let interperet ship { typ; num } =
  let open Complex in
  let ( + ) = Complex.add in
  let ( - ) = Complex.sub in
  let ( * ) s c =
    let s = Float.of_int s in
    Complex.{ re = c.re *. s ; im = c.im *. s;}
  in
  let update_dir c num =
    List.init ~f:(const c) (num / 90)
    |> List.fold ~f:mul ~init:ship.dir
  in
   match typ with
  | `N -> { ship with pos = ship.pos + num * i }
  | `S -> { ship with pos = ship.pos - num * i }
  | `E -> { ship with pos = ship.pos + num * one }
  | `W -> { ship with pos = ship.pos - num * one }
  | `F -> { ship with pos = ship.pos + num * ship.dir }
  | `L -> { ship with dir = update_dir i num }
  | `R -> { ship with dir = update_dir (neg i) num }

let rain_risk input =
  parse_instr input
  |> List.fold  ~init:starting_ship ~f:interperet
  |> taxicab

let () =
  In_channel.create "./src/aoc12/input.txt"
  |> In_channel.input_all
  |> rain_risk
  |> Stdio.printf "%d\n"