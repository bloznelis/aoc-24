let parse input = Base.String.split_lines input |> Array.of_list |> Array.map Base.String.to_array
let calc_antinodes (x1, y1) (x2, y2) = [ (x1 + x1 - x2, y1 + y1 - y2) ]

let calc_antinodes_2 matrix (x1, y1) (x2, y2) =
  let x_diff, y_diff = (x1 - x2, y1 - y2) in
  let rec go acc (x, y) =
    let new_coords = (x + x_diff, y + y_diff) in
    if Util.safe_get_a new_coords matrix |> Option.is_some then go (new_coords :: acc) new_coords
    else acc
  in
  go [] (x2, y2)

let group_antennas matrix =
  let table = Base.Hashtbl.create (module Base.Char) in
  let update key value =
    Base.Hashtbl.update table key ~f:(fun maybe_value ->
        match maybe_value with Some coll -> value :: coll | None -> [ value ])
  in
  Array.iteri
    (fun y row -> Array.iteri (fun x antenna -> if antenna != '.' then update antenna (x, y)) row)
    matrix;
  table

let pair_up elem list =
  let rec go rem acc =
    match rem with
    | head :: tail when head = elem -> go tail acc
    | head :: tail -> if head = elem then go tail acc else go tail ((elem, head) :: acc)
    | [] -> acc
  in
  go list []

let pair_antennas antennas = List.concat_map (fun elem -> pair_up elem antennas) antennas

let filter_outside matrix antinodes =
  List.filter (fun antinode -> Util.safe_get_a antinode matrix |> Option.is_some) antinodes

let find_antinodes antennas _ =
  pair_antennas antennas |> List.concat_map (fun (a, b) -> calc_antinodes a b)

let find_anitnodes_2 antennas matrix =
  pair_antennas antennas |> List.concat_map (fun (a, b) -> calc_antinodes_2 matrix a b)

let solve antinodes_fn matrix =
  group_antennas matrix
  |> Base.Hashtbl.map ~f:(fun antennas -> antinodes_fn antennas matrix)
  |> Base.Hashtbl.data |> List.flatten |> Base.List.stable_dedup ~compare |> filter_outside matrix
  |> List.length

let part1 input = input |> parse |> solve find_antinodes |> string_of_int
let part2 input = input |> parse |> solve find_anitnodes_2 |> string_of_int
