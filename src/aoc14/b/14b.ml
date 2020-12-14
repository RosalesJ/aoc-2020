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
  List.rev (loop (Int64.of_string str) [])

let parse_data input =
  let num = take_while Char.is_digit in
  let mask_digit = char '0' <|> char '1' <|> char 'X' in
  let mask = string "mask = " *> many mask_digit >>| fun x -> First (List.rev x) in
  let set = lift2 (fun x y -> Second (to_bits x, Int64.of_string y)) (string "mem[" *> num <* string "] = ") num in
  let line = set <|> mask in
  match parse_string (sep_by (char '\n') line) input ~consume:All with
  | Ok xs -> xs
  | Error e -> printf "%s\n" e; []

let all_addresses input =
  let rec loop cur (acc: char list list) = function
    | [] -> (List.rev cur) :: acc
    | 'X' :: xs -> List.append (loop ('0' :: cur) acc xs) (loop ('1' :: cur) acc xs)
    | x :: xs -> loop (x :: cur) acc xs
in
loop [] [] input

let docking_data input =
  let adresses = Map.empty (module Int64) in
  let data = parse_data input in
  let empty_mask = List.init 36  ~f:(const 'X') in
  let apply_mask mask adress =
    printf "val: ";
    (List.iter ~f:(printf "%c")) adress;
    printf " -- %Ld\nmas: " (to_num adress);
    (List.iter ~f:(printf "%c")) mask;

    let merge = List.map ~f:(function
      | '0', x -> x
      | x, _ -> x)
    in

    let merged = match List.zip_with_remainder mask adress with
    | l, Some (First xs) -> List.append (merge l) xs
    | l, _ -> merge l in

    printf "\nmrg: ";
    (List.iter ~f:(printf "%c")) merged;
    printf "\nall:\n";
    (List.iter ~f:(fun x -> List.iter ~f:(printf "%c") x; print_endline "")) (all_addresses merged);
    merged
  in

  let rec loop adresses mask instructions =
    match mask, instructions with
    | _, [] -> adresses
    | _, (First mask) :: xs -> loop adresses mask xs
    | mask, Second (adress, value) :: xs ->
      let to_store = apply_mask mask adress in
      let all = all_addresses to_store in
      let adresses = List.fold ~init:adresses ~f:(fun acc x -> Map.update acc (to_num x) ~f:(const value)) all in
      loop adresses mask xs
  in
  let open Int64 in

  loop adresses empty_mask data
  |> Map.fold ~init:(Int64.of_int 0) ~f:(fun ~key ~data acc ->
    printf "%Ld: %Ld -- " key data;
    print_endline "";
    acc + data)

let () =
  In_channel.create "./src/aoc14/input.txt"
  |> In_channel.input_all
  |> docking_data
  |> Stdio.printf "%Ld\n"