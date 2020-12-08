open Core
open Stdio
open Angstrom

type instruction = { typ: [`Jmp | `Acc | `Nop]; num: int; visited: bool }

let parse_instruction input =
  let typ = take 3 >>| function "acc" -> `Acc | "jmp" -> `Jmp | "nop" -> `Nop | _ -> assert false in
  let sign = char ' ' *> (char '+' *> return 1 <|> char '-' *> return (-1)) in
  let num = lift2 (fun s n -> s * Int.of_string n) sign (take_while Char.is_digit) in
  let instruction = lift2 (fun typ num -> { typ; num; visited = false }) typ num in

  match parse_string (sep_by (char '\n') instruction) input ~consume:Prefix with
  | Ok xs -> xs
  | Error x -> printf "%s\n" x; []

let handheld_halting input =
  let instructions = Array.of_list (parse_instruction input) in

  let rec loop acc pos =
    let next = instructions.(pos) in
    if next.visited then
      acc
    else begin
      instructions.(pos) <- { next with visited = true };
      match next.typ with
      | `Acc -> loop (acc + next.num) (pos + 1)
      | `Nop -> loop acc (pos + 1)
      | `Jmp -> loop acc (pos + next.num)
    end
  in
  loop 0 0

let () =
  In_channel.create "./src/aoc8/input.txt"
  |> In_channel.input_all
  |> handheld_halting
  |> Stdio.printf "%d\n"
