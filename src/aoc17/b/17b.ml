open Core

let count_neighbors cubes (px, py, pz, pw) =
  let clamp k = k >= 0 && k < Array.length cubes in
  let around k = (Sequence.range (k - 1) (k + 1) ~start:`inclusive ~stop:`inclusive) in
  Sequence.cartesian_product (around px) (Sequence.cartesian_product (around py) (Sequence.cartesian_product (around pz) (around pw)))
  |> Sequence.filter_map ~f:(fun (x,(y, (z, w))) -> Option.some_if (not (x = px && y = py && z = pz && w = pw)) (x, y, z, w))
  |> Sequence.count ~f:(fun (x, y, z, w) ->
    clamp x && clamp y && clamp z && clamp w && Char.equal '#' cubes.(w).(z).(y).(x))

let conway_cubes iters input =
  let input = List.map input ~f:(fun x -> String.to_array x) |> List.to_array in
  let size = Array.length input + iters * 2 in
  let cubes = Array.make_matrix ~dimx:size ~dimy:size (Array.make_matrix ~dimx:size ~dimy:size '.') in

  let input_size = Array.length input in
  let center = Array.length cubes / 2 in
  let cubes = Array.mapi cubes ~f:(fun w -> Array.mapi ~f:(fun z -> Array.mapi ~f:(fun y -> Array.mapi ~f:(fun x a ->
    if z = center && w = center && y >= iters && y < iters + input_size && x >= iters && x < iters + input_size then
      input.(y - iters).(x - iters)
    else a))))
  in

  let rec _loop gens cubes =
    if gens = iters then
      cubes
    else
      let cubes = Array.mapi cubes ~f:(fun w -> Array.mapi ~f:(fun z -> Array.mapi ~f:(fun y -> Array.mapi ~f:(fun x a ->
        match a, count_neighbors cubes (x, y, z, w) with
        | '#', 2 | '#', 3 -> '#'
        | '#', _ -> '.'
        | '.', 3 -> '#'
        | _ -> '.'))))
      in
      _loop (gens + 1) cubes
  in

  _loop 0 cubes
  |> Array.sum (module Int) ~f:(Array.sum (module Int) ~f:(Array.sum (module Int) ~f:(Array.count ~f:(Char.equal '#'))))

let () =
  In_channel.create "./src/aoc17/input.txt"
  |> In_channel.input_lines
  |> conway_cubes 6
  |> Format.printf "%d\n"