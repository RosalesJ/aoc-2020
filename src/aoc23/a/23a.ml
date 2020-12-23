open Core

let crab_cups x input =
  let buf = String.to_array input |> Array.map ~f:(fun x -> String.of_char x |> Int.of_string) in
  let size = Array.length buf in
  let grab_size = 3 in
  let (+|) x y = (x + y + size) mod size in
  let grab = Array.create ~len:grab_size 0 in

  let rec get_target i =
    let i = ((i - 2 + size) mod size) + 1 in
    if not (Array.exists grab ~f:(Int.equal i)) then
      i
    else
      get_target i
  in
  print_endline "entering";

  for i = 0 to x - 1 do
    printf "round %d, selected %d\n" (i + 1) (buf.(i mod size));
    printf "cups: %s\n" (String.concat ~sep:", " (Array.to_list (Array.map buf ~f:Int.to_string)));
    Stdlib.flush Stdlib.stdout;
    let cur = i mod size in
    (* grab the next three *)
    for i = 0 to grab_size - 1 do
      grab.(i) <- buf.(cur +| (i + 1))
    done;
    printf "pickup: %s\n" (String.concat ~sep:", " (Array.to_list (Array.map grab ~f:Int.to_string)));
    let grab_start = cur +| 1 in
    (* let grab_end = cur +| grab_size in *)
    let dest_label = get_target (buf.(i mod size)) in
    printf "dest label: %d\n" dest_label;
    let dest = Array.find_mapi_exn buf ~f:(fun i x -> Option.some_if (x = dest_label) i) in
    (* slide everything back *)
    for i = grab_start to grab_start + ((dest - grab_start - grab_size) +| size) do
      printf "b %d\n" i;
      Stdlib.flush Stdlib.stdout;
      buf.(i mod size) <- buf.(i +| grab_size)
    done;
    (* replace grab group*)
    for i = 0 to grab_size - 1 do
      printf "c %d\n" ((dest - grab_size) +| i);
      Stdlib.flush Stdlib.stdout;
      buf.((dest - grab_size + 1) +| i) <- grab.(i)
    done;
  done;

  let final = Array.create ~len:(size - 1) 0 in
  let one_index = Array.find_mapi_exn buf ~f:(fun i x -> Option.some_if (x = 1) i) in
  printf "one_index %d\n" one_index;
  for i = 0 to size - 2 do
    final.(i) <- buf.((one_index + i + size + 1) mod size);
    printf "%d: %d\n" ((one_index + i + size + 1) mod size) final.(i)
  done;

  String.concat (Array.map final ~f:Int.to_string |> Array.to_list)



let () =
  In_channel.create "./src/aoc23/input.txt"
  |> In_channel.input_all
  |> crab_cups 100
  |> Format.printf "%s\n"