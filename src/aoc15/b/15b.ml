open Core

type track = First of int | Mult of int * int

let rambunctiuos_recitation target input =
  let update i = Map.update ~f:(function None -> First i
  | Some (First fst) -> Mult (i, fst)
  | Some (Mult (fst, _)) -> Mult (i, fst))
  in

  let input = String.split input ~on:','
    |> List.map ~f:Int.of_string
  in

  let map = List.foldi ~init:(Map.empty (module Int)) ~f:(fun i acc x -> update (i + 1) acc x) input in

  let rec loop visited i last =
    if i mod 1000000 = 0 then begin
      printf "%d %d\n" i last;
      Stdio__Out_channel.flush Out_channel.stdout;
    end;
    if i = target then
      last
    else
      match Map.find_exn visited last with
      | First _ ->
        loop (update (i + 1) visited 0) (i + 1) 0
      | Mult (turn, prev) ->
        let say = turn - prev in
        loop (update (i + 1) visited say) (i + 1) say
  in

  let last = List.last_exn input in
  let i = List.length input in
  loop map i last

let () =
  In_channel.create "./src/aoc15/input.txt"
  |> In_channel.input_all
  |> rambunctiuos_recitation 30000000
  |> Format.printf "%d\n"