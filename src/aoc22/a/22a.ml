open Core
open Angstrom

let parse_input input =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  let deck = take_till (Char.equal ':') *> string ":\n" *> sep_by end_of_line num >>| Queue.of_list in
  let decks = lift2 Tuple2.create deck (end_of_line *> end_of_line *> deck) in
  match parse_string decks input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let crab_combat input =
  let q1, q2 = parse_input input in

  let rec play q1 q2 =
    if Queue.is_empty q1 then
      q2
    else if Queue.is_empty q2 then
      q1
    else
      let play1 = Queue.dequeue_exn q1 in
      let play2 = Queue.dequeue_exn q2 in
      if play1 > play2 then
         (Queue.enqueue q1 play1;
          Queue.enqueue q1 play2;
          play q1 q2)
      else
         (Queue.enqueue q2 play2;
          Queue.enqueue q2 play1;
          play q1 q2)
    in
    let winner = play q1 q2 in
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