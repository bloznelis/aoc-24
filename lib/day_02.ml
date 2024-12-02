let validate line cmp =
    List.fold_left2
      (fun acc a b -> acc && cmp a b && Int.abs (a - b) < 4 && Int.abs (a - b) > 0)
      true (Util.drop_last line) (List.tl line)

let explode line =
    let rec go past rem acc =
        match rem with
            | [] -> acc
            | hd :: tl -> go (hd :: past) tl (List.append (List.rev past) tl :: acc)
    in
        line :: go [] line []

let is_decreasing line = validate line ( > )
let is_increasing line = validate line ( < )
let is_decreasing_2 line = List.map is_decreasing (explode line) |> List.fold_left ( || ) false
let is_increasing_2 line = List.map is_increasing (explode line) |> List.fold_left ( || ) false

let solve input filter =
    Base.String.split_lines input
    |> List.map (fun line -> line |> String.split_on_char ' ' |> List.map int_of_string)
    |> List.filter filter |> List.length |> string_of_int

let part1 input = solve input (fun line -> is_increasing line || is_decreasing line)
let part2 input = solve input (fun line -> is_increasing_2 line || is_decreasing_2 line)
