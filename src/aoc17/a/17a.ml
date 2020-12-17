open Core

let count_neighbors cubes (px, py, pz) =
  let clamp k = k >= 0 && k < Array.length cubes in
  let around k = (Sequence.range (k - 1) (k + 1) ~start:`inclusive ~stop:`inclusive) in
  Sequence.cartesian_product (around px) (Sequence.cartesian_product (around py) (around pz))
  |> Sequence.filter_map ~f:(fun (x,(y, z)) -> Option.some_if (not (x = px && y = py && z = pz)) (x, y, z))
  |> Sequence.count ~f:(function x, y, z ->
    clamp x && clamp y && clamp z && (* ((printf "(%d, %d, %d): %c\n" x y z cubes.(z).(y).(x)); *) Char.equal '#' cubes.(z).(y).(x))

let conway_cubes iters input =
  let input = List.map input ~f:(fun x -> String.to_array x) |> List.to_array in
  let size = Array.length input + iters * 2 in
  let cubes = Array.create ~len:size (Array.make_matrix ~dimx:size ~dimy:size '.') in

  let input_size = Array.length input in
  let center = Array.length cubes / 2 in
  let cubes = Array.mapi cubes ~f:(fun z -> Array.mapi ~f:(fun y -> Array.mapi ~f:(fun x a ->
    if z = center && y >= iters && y < iters + input_size && x >= iters && x < iters + input_size then
      ((printf "(%d %d %d) trying :(%d, %d)\n" x y z (y - iters) (x - iters));
      input.(y - iters).(x - iters))
    else a)))
  in

  let rec _loop gens cubes =
    if gens = iters then
      cubes
    else
      (ignore (count_neighbors cubes (7, 7, 7));
      let cubes = Array.mapi cubes ~f:(fun z -> Array.mapi ~f:(fun y -> Array.mapi ~f:(fun x a ->
        match a, count_neighbors cubes (x, y, z) with
        | '#', 2 | '#', 3 -> '#'
        | '#', _ -> '.'
        | '.', 3 -> '#'
        | _ -> '.')))
      in
      _loop (gens + 1) cubes)
  in

  _loop 0 cubes
  |> Array.sum (module Int) ~f:(Array.sum (module Int) ~f:(Array.count ~f:(Char.equal '#')))

let () =
  In_channel.create "./src/aoc17/input.txt"
  |> In_channel.input_lines
  |> conway_cubes 6
  |> Format.printf "%d\n"