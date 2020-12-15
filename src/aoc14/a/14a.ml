open Core
open Angstrom

let and_mask =
  let open Int63 in
  String.foldi ~init:(of_int 0) ~f:(fun i acc -> function
  | 'X' | '1' -> acc lor ((of_int 1) lsl i)
  | _ -> acc)

let or_mask =
  let open Int63 in
  String.foldi ~init:(of_int 0) ~f:(fun i acc -> function
  | '1' -> acc lor ((of_int 1) lsl i)
  | _ -> acc)

let parse_data input =
  let num = take_while Char.is_digit in
  let is_mask_digit = function '0' | '1' | 'X' -> true | _ -> false in
  let mask = string "mask = " *> take_while is_mask_digit >>| fun x -> First (String.rev x) in
  let set = lift2 (fun x y -> Second (Int63.of_string x, Int63.of_string y)) (string "mem[" *> num <* string "] = ") num in
  let line = set <|> mask in
  match parse_string (sep_by (char '\n') line) input ~consume:All with
  | Ok xs -> xs
  | Error e -> printf "%s\n" e; []

let docking_data input =
  let adresses = Map.empty (module Int63) in
  let open Int63 in
  let data = parse_data input in
  let empty_mask = "0" in
  let apply_mask mask value = (value land (and_mask mask)) lor (or_mask mask) in

  let rec loop adresses mask instructions =
    match mask, instructions with
    | _, [] -> adresses
    | _, (First mask) :: xs -> loop adresses mask xs
    | mask, Second (address, value) :: xs ->
      let to_store = apply_mask mask value in
      let adresses = Map.update adresses address ~f:(const to_store) in
      loop adresses mask xs
  in

  loop adresses empty_mask data
  |> Map.data
  |> List.sum (module Int63) ~f:Fn.id

let () =
  In_channel.create "./src/aoc14/input.txt"
  |> In_channel.input_all
  |> docking_data
  |> Format.printf "%a\n" Int63.pp