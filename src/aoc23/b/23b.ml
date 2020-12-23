open Core

type cup = { label: int; mutable next: cup option }

let rec go_forward cup n =
  if n = 0 then cup
  else go_forward (Option.value_exn cup.next) (n - 1)

let labels_between ?(inclusive = false) cupa cupb =
  let rec loop cupa cupb acc inclusive =
    if cupa.label = cupb.label then
      if inclusive then
        cupa.label :: acc
      else
        acc
    else
      let acc = cupa.label :: acc in
      let nexta = Option.value_exn cupa.next in
      loop nexta cupb acc inclusive
  in
  List.rev (loop cupa cupb [] inclusive)

let serialize cup =
  let last_labels = labels_between (Option.value_exn cup.next) cup ~inclusive:true in
  String.concat ~sep:", " (List.map (last_labels) ~f:Int.to_string)

let crab_cups turns size input =
  let buf = String.to_array input |> Array.map ~f:(fun x -> String.of_char x |> Int.of_string) in
  let buf = Array.init size ~f:(fun i -> if i < Array.length buf then buf.(i) else i + 1) in
  let lst = { next = None; label = buf.(Array.length buf - 1) } in
  let circle = Array.fold_right (Array.slice buf 0 (Array.length buf - 1)) ~init:lst ~f:(fun label next -> {label; next = Some next}) in
  let cur = circle in
  lst.next <- Some cur;

  let cups_by_label =
    let dest = cur.label in
    let rec loop cur acc =
      if cur.label = dest then
        acc
      else
        let acc = Map.add_exn acc ~key:cur.label ~data:cur in
        loop (Option.value_exn cur.next) acc
    in
    loop (Option.value_exn cur.next) (Map.singleton (module Int) cur.label cur)
  in

  let grab_size = 3 in

  let rec get_target grab_group i =
    let i = ((i - 2 + size) mod size) + 1 in
    if not (List.exists grab_group ~f:(Int.equal i)) then
      i
    else
      get_target grab_group i
  in

  let rec loop i current_cup =
    if i = turns then
      current_cup
    else begin
      (* printf "\nselected: %d\n" current_cup.label;
      print_endline (serialize cur); *)
      let first_grabbed = Option.value_exn current_cup.next in

      (* grab the next three *)
      let last_grabbed = go_forward first_grabbed (grab_size - 1) in
      let labels_grabbed = labels_between first_grabbed (Option.value_exn last_grabbed.next) in

      (* printf "grabbed: %s\n" (String.concat (List.map labels_grabbed ~f:Int.to_string)); *)

      (* get destination label*)
      let dest_label = get_target labels_grabbed current_cup.label in
      let dest = Map.find_exn cups_by_label dest_label in
      (* printf "dest: %d\n" dest_label; *)

      (* slide everything back *)
      current_cup.next <- last_grabbed.next;
      last_grabbed.next <- dest.next;
      dest.next <- Some first_grabbed;

      loop (i + 1) (Option.value_exn current_cup.next)
    end
  in
  ignore (loop 0 cur);

  let one_index = Map.find_exn cups_by_label 1 in
  let after_one = Option.value_exn one_index.next in
  let two_after_one = Option.value_exn after_one.next in
  printf "%d, %d\n" after_one.label two_after_one.label;
  after_one.label * two_after_one.label




let () =
  In_channel.create "./src/aoc23/input.txt"
  |> In_channel.input_all
  |> crab_cups 10_000_000 1_000_000
  |> Format.printf "%d\n"