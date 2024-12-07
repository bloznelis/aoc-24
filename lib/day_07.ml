let parse input =
  Base.String.split_lines input
  |> List.map (fun line -> Util.re_seq "[0-9]+" line |> List.map int_of_string)

let concat a b = String.concat "" [ string_of_int a; string_of_int b ] |> int_of_string

let is_valid with_concat operation =
  let expected = List.hd operation in
  let rec go rem acc =
    match rem with
    | [] -> acc == expected
    | head :: tail ->
        go tail (acc * head) || go tail (acc + head) || (with_concat && go tail (concat acc head))
  in
  go (List.tl operation) 0

let solve concat input =
  input |> parse |> List.filter (is_valid concat) |> List.map List.hd |> Util.sum |> string_of_int

let part1 input = solve false input
let part2 input = solve true input
