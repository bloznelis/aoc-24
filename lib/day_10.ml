type coords = int * int

let parse input =
  Base.String.split_lines input |> Array.of_list
  |> Array.map (fun line ->
         Base.String.to_array line |> Array.map (fun char -> Base.Char.get_digit char |> Option.get))

let valid_steps (from_x, from_y) matrix : coords list =
  let point_height = matrix.(from_y).(from_x) in
  let options =
    [ (from_x + 1, from_y); (from_x - 1, from_y); (from_x, from_y + 1); (from_x, from_y - 1) ]
  in
  List.filter_map
    (fun coords ->
      match Util.safe_get_a coords matrix with
      | Some height when point_height + 1 = height -> Some coords
      | _ -> None)
    options

let explore point matrix : int =
  let rec go (x, y) =
    let point_height = matrix.(y).(x) in
    if point_height = 9 then [ (x, y) ]
    else
      let valid = valid_steps (x, y) matrix in
      Base.List.concat_map valid ~f:(fun next_point -> go next_point)
  in
  go point |> Base.List.stable_dedup ~compare |> List.length

let rec explore_2 (point_x, point_y) matrix : int =
  let point_height = matrix.(point_y).(point_x) in
  if point_height = 9 then 1
  else
    let valid = valid_steps (point_x, point_y) matrix in
    List.fold_left (fun acc elem -> acc + explore_2 elem matrix) 0 valid

let solve explore_fn matrix =
  Array.mapi
    (fun y row ->
      Array.mapi (fun x height -> if height = 0 then explore_fn (x, y) matrix else 0) row
      |> Array.to_list)
    matrix
  |> Array.to_list |> List.flatten |> Util.sum

let part1 input = input |> parse |> solve explore |> string_of_int
let part2 input = input |> parse |> solve explore_2 |> string_of_int
