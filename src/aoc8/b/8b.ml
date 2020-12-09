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
  let instructions = parse_instruction input in
  let jmp_and_nops = List.filter_mapi instructions ~f:(fun i x -> match x.typ with `Jmp | `Nop -> Some (i, x) | _ -> None) in
  let instructions = Map.of_alist_exn (module Int) (List.mapi ~f:(fun i x -> (i, x)) instructions) in

  let flip_type (num, instr) =
    let flip_in c = Map.update instructions num ~f:(Fn.const c) in
    match instr.typ with
    | `Acc -> instructions
    | `Jmp -> flip_in { instr with typ = `Nop }
    | `Nop -> flip_in { instr with typ = `Jmp }
  in

  let rec loop instructions acc pos =
    if pos = Map.length instructions then
      Some acc
    else if pos < 0 || Map.length instructions < pos then
      failwith "Out of range"
    else begin
      let next = Map.find_exn instructions pos in
      if next.visited then
        None
      else begin
        let instructions = Map.update instructions pos ~f:(Fn.const { next with visited = true }) in
        match next.typ with
        | `Acc -> loop instructions (acc + next.num) (pos + 1)
        | `Nop -> loop instructions acc (pos + 1)
        | `Jmp -> loop instructions acc (pos + next.num)
      end
    end
  in

  match List.find_map jmp_and_nops ~f:(fun x -> loop (flip_type x) 0 0) with
  | Some x -> x
  | None -> print_endline "nothing :("; 0


let () =
  In_channel.create "./src/aoc8/input.txt"
  |> In_channel.input_all
  |> handheld_halting
  |> Stdio.printf "%d\n"
