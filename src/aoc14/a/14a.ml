open Core
open Stdio
open Angstrom

let to_num =
  let open Int64 in
  List.foldi ~init:(of_int 0) ~f:(fun i acc -> function
  | '1' -> acc + (pow (of_int 2) (of_int i))
  | _ -> acc)

let to_bits str =
  let open Int64 in
  let rec loop num acc =
    if num = (of_int 0) then
      acc
    else
      let rem = Char.of_string (sprintf "%d" ((Int64.to_int_exn num) mod 2)) in
      loop (num / (of_int 2)) (rem :: acc)
    in
  loop (Int64.of_string str) []

let parse_data input =
  let num = take_while Char.is_digit in
  let mask_digit = char '0' <|> char '1' <|> char 'X' in
  let mask = string "mask = " *> many mask_digit >>| fun x -> First x in
  let set = lift2 (fun x y -> Second (Int64.of_string x, to_bits y)) (string "mem[" *> num <* string "] = ") num in
  let line = set <|> mask in
  match parse_string (sep_by (char '\n') line) input ~consume:All with
  | Ok xs -> xs
  | Error e -> printf "%s\n" e; []

let docking_data input =
  let adresses = Map.empty (module Int64) in
  let data = parse_data input in
  let empty_mask = List.init 36  ~f:(const 'X') in
  let apply_mask mask value =
    printf "val: ";
    (List.iter ~f:(printf "%c")) (List.rev value);
    printf "\nmas: ";
    (List.iter ~f:(printf "%c")) (List.rev mask);

    let merge = List.map ~f:(function
      | 'X', x -> x
      |  m, _ -> m)
    in

    let remainder = List.map ~f:(function
      | 'X' -> '0'
      | x -> x)
    in

    let merged = match List.zip_with_remainder (List.rev mask) (List.rev value) with
    | l, Some (First xs) -> List.append (merge l) (remainder xs)
    | l, _ -> merge l in

    printf "\nmrg: ";
    (List.iter ~f:(printf "%c")) merged;
    print_endline "\n";
    merged
  in

  let rec loop adresses mask instructions =
    match mask, instructions with
    | _, [] -> adresses
    | _, (First mask) :: xs -> loop adresses mask xs
    | mask, Second (address, value) :: xs ->
      let to_store = apply_mask mask value in
      let adresses = Map.update adresses address ~f:(const to_store) in
      loop adresses mask xs
  in
  let open Int64 in

  loop adresses empty_mask data
  |> Map.fold ~init:(Int64.of_int 0) ~f:(fun ~key ~data acc ->
    printf "%Ld: %Ld -- " key (to_num data);
    (List.iter ~f:(printf "%c")) data;
    print_endline "";
    acc + (to_num data))

let () =
  In_channel.create "./src/aoc14/input.txt"
  |> In_channel.input_all
  |> docking_data
  |> Stdio.printf "%Ld\n"