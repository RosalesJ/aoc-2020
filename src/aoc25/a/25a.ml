open Core

let find_loop_size x p t =
  (* find k with x ^ k mod p = t *)
  let rec loop n k =
    if n = t then
      k
    else
      let n = (x * n) mod p in
      loop n (k + 1)
  in
  loop 1 0

let transform s k p =
  (* calculate n with n = s^k mod p*)
  let rec loop n k =
    if k = 0 then
      n
    else
      let n = (s * n) mod p in
      loop n (k - 1)
  in
  loop 1 k


let combo_breaker p input =
  let x = 7 in
  let door_pkey = List.hd_exn input in
  let card_pkey = List.nth_exn input 1 in

  let door_loop = find_loop_size x p door_pkey in
  let card_loop = find_loop_size x p card_pkey in

  let card_enc = transform door_pkey card_loop p in
  let door_enc = transform card_pkey door_loop p in

  printf "%d %d\n" door_loop card_loop;
  printf "%d %d\n" card_enc door_enc;

  1


let () =
  In_channel.create "./src/aoc25/input.txt"
  |> In_channel.input_lines
  |> List.map ~f:Int.of_string
  |> combo_breaker 20201227
  |> Format.printf "%d\n"