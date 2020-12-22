open Core
open Angstrom

let parse_input input =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  let deck = take_till (Char.equal ':') *> string ":\n" *> sep_by end_of_line num in
  let decks = lift2 Tuple2.create deck (end_of_line *> end_of_line *> deck) in
  match parse_string decks input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let crab_combat input =
  let (p1, p2) = parse_input input in
  let q1, q2 = Queue.of_list p1, Queue.of_list p2 in

  let rec play q1 q2 =
    print_endline "";
    printf "Player 1's deck: %s\n" (String.concat ~sep:", " (q1 |> Queue.to_list |> List.map ~f:Int.to_string));
    printf "Player 2's deck: %s\n" (String.concat ~sep:", " (q2 |> Queue.to_list |> List.map ~f:Int.to_string));
    if Queue.is_empty q1 then
      q2
    else if Queue.is_empty q2 then
      q1
    else
      let play1 = Queue.dequeue_exn q1 in
      let play2 = Queue.dequeue_exn q2 in
      printf "Player 1 plays: %s\n" (Int.to_string play1);
      printf "Player 2 plays: %s\n" (Int.to_string play2);
      if play1 > play2 then
        (
          print_endline "Player 1 wins!";
          Queue.enqueue q1 play1;
          Queue.enqueue q1 play2;
          play q1 q2)
      else
        (
          print_endline "Player 2 wins!";
          Queue.enqueue q2 play2;
          Queue.enqueue q2 play1;
          play q1 q2)
    in
    let winner = play q1 q2 in

    print_endline "\n\n== Post-game results: ==";
    printf "Player 1's deck: %s\n" (String.concat ~sep:", " (List.map p1 ~f:Int.to_string));
    printf "Player 2's deck: %s\n" (String.concat ~sep:", " (List.map p2 ~f:Int.to_string));
    Queue.to_list winner
    |> List.rev
    |> List.zip_exn (List.range 1 (Queue.length winner + 1))
    |> List.map ~f:(Tuple2.uncurry ( * ))
    |> List.sum (module Int) ~f:ident


let () =
  In_channel.create "./src/aoc22/input.txt"
  |> In_channel.input_all
  |> crab_combat
  |> Format.printf "%d\n"