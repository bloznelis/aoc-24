let sveikas f = f = Float.trunc f

let solve_linear_system a1 b1 c1 a2 b2 c2 =
  let det = (a1 *. b2) -. (a2 *. b1) in
  if det = 0.0 then None
  else
    let x = ((c1 *. b2) -. (c2 *. b1)) /. det in
    let y = ((a1 *. c2) -. (a2 *. c1)) /. det in
    Some (x, y)

let calc (ax, ay) (bx, by) (x, y) =
  match solve_linear_system ax bx x ay by y with
  | Some (a, b) when sveikas a && sveikas b -> Some ((int_of_float a * 3) + int_of_float b)
  | _ -> None

let solve offset parsed =
  let rec go rem acc =
    match rem with
    | first :: second :: (target_x, target_y) :: tail ->
        go tail (calc first second (target_x +. offset, target_y +. offset) :: acc)
    | [] -> Base.List.filter_opt acc |> Util.sum
    | _ -> failwith "bad input"
  in
  go parsed []

let parse input =
  Base.String.split_lines input
  |> List.filter (fun x -> Base.String.is_empty x |> Bool.not)
  |> List.map (fun line ->
         match Util.re_seq "[0-9]+" line with
         | [ x; y ] -> (float_of_string x, float_of_string y)
         | _ -> failwith "bad input")

let part1 input = input |> parse |> solve 0. |> string_of_int
let part2 input = input |> parse |> solve 10000000000000. |> string_of_int
