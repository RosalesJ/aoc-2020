open Core
open Angstrom

let parse_input input =
  let num = take_while1 Char.is_digit >>| Int.of_string in
  let deck = take_till (Char.equal ':') *> string ":\n" *> sep_by end_of_line num >>| Fqueue.of_list in
  let decks = lift2 Tuple2.create deck (end_of_line *> end_of_line *> deck) in
  match parse_string decks input ~consume:All with
  | Ok x -> x
  | Error e -> failwith e

let take q n = Fqueue.to_list q |> Fn.flip List.take n |> Fqueue.of_list

let crab_combat input =
  let q1, q2 = parse_input input in

  let rec play q1 q2 visited =
    let sexp = List.sexp_of_t (Fqueue.sexp_of_t Int.sexp_of_t) [q1; q2] in

    if Set.mem visited sexp then
      `P1, q1
    else if Fqueue.is_empty q1 then
      `P2, q2
    else if Fqueue.is_empty q2 then
      `P1, q1
    else
      let visited = Set.add visited sexp in
      let card1, q1 = Fqueue.dequeue_exn q1 in
      let card2, q2 = Fqueue.dequeue_exn q2 in

      let p1wins = ref (if card1 > card2 then `P1 else `P2) in

      if card1 <= Fqueue.length q1 && card2 <= Fqueue.length q2 then begin
        let winner, _ = play (take q1 card1) (take q2 card2) visited in
        p1wins := winner
      end;

      match !p1wins with
      | `P1 ->
        let q1 = Fqueue.enqueue q1 card1 in
        let q1 = Fqueue.enqueue q1 card2 in
        play q1 q2 visited
      | `P2 ->
        let q2 = Fqueue.enqueue q2 card2 in
        let q2 = Fqueue.enqueue q2 card1 in
        play q1 q2 visited
    in

    let _, winning_deck = play q1 q2 (Set.empty (module Sexp)) in
    Fqueue.to_list winning_deck
    |> List.rev
    |> List.zip_exn (List.range 1 (Fqueue.length winning_deck + 1))
    |> List.map ~f:(Tuple2.uncurry ( * ))
    |> List.sum (module Int) ~f:ident

let () =
  In_channel.create "./src/aoc22/input.txt"
  |> In_channel.input_all
  |> crab_combat
  |> Format.printf "%d\n"