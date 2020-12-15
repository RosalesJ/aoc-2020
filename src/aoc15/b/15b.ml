open Core

let rambunctious_recitation target input =
  let input = List.map ~f:Int.of_string @@ String.split input ~on:',' in
  let arr = Array.init (target + 1) ~f:(const None) in
  List.iteri input ~f:(fun i x -> arr.(x) <- Some (i + 1));

  let rec loop turn last =
    if turn > target then
      last
    else
      let say =
        match arr.(last) with
        | None -> 0
        | Some x -> (turn - 1) - x
      in
      arr.(last) <- Some (turn - 1);
      loop (turn + 1) say
  in
  loop (List.length input + 1) (List.last_exn input)

let () =
  In_channel.create "./src/aoc15/input.txt"
  |> In_channel.input_all
  |> rambunctious_recitation 30000000
  |> Format.printf "%d\n"