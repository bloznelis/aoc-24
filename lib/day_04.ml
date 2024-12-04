let parse input : char list list = Base.String.split_lines input |> List.map Util.to_charlist

let paths (x, y) : (int * int) list list =
    [
      [ (x, y); (x + 1, y); (x + 2, y); (x + 3, y) ];
      [ (x, y); (x - 1, y); (x - 2, y); (x - 3, y) ];
      [ (x, y); (x, y + 1); (x, y + 2); (x, y + 3) ];
      [ (x, y); (x, y - 1); (x, y - 2); (x, y - 3) ];
      [ (x, y); (x + 1, y + 1); (x + 2, y + 2); (x + 3, y + 3) ];
      [ (x, y); (x - 1, y - 1); (x - 2, y - 2); (x - 3, y - 3) ];
      [ (x, y); (x + 1, y - 1); (x + 2, y - 2); (x + 3, y - 3) ];
      [ (x, y); (x - 1, y + 1); (x - 2, y + 2); (x - 3, y + 3) ];
    ]
    |> List.filter (List.fold_left (fun acc (x, y) -> acc && x >= 0 && y >= 0) true)

let paths_2 (x, y) : (int * int) list list =
    [
      [ (x - 1, y + 1); (x, y); (x + 1, y - 1) ]; [ (x + 1, y - 1); (x, y); (x - 1, y + 1) ];
      [ (x + 1, y + 1); (x, y); (x - 1, y - 1) ]; [ (x - 1, y - 1); (x, y); (x + 1, y + 1) ];
    ]
    |> List.filter (List.fold_left (fun acc (x, y) -> acc && x >= 0 && y >= 0) true)

let get_in_path paths matrix = List.map (fun coords -> Util.safe_get coords matrix) paths

let validate = function
    | [ Some 'X'; Some 'M'; Some 'A'; Some 'S' ] -> true
    | _ -> false

let validate_2 = function
    | [ Some 'M'; Some 'A'; Some 'S' ] -> true
    | _ -> false

let solve matrix =
    Util.coords matrix |> List.map paths
    |> List.map (fun paths ->
           List.filter (fun path -> get_in_path path matrix |> validate) paths |> List.length)
    |> Util.sum

let solve_2 matrix =
    Util.coords matrix |> List.map paths_2
    |> List.map (fun paths ->
           if
             List.filter (fun path -> get_in_path path matrix |> validate_2) paths
             |> List.length == 2
           then
             1
           else
             0)
    |> Util.sum

let part1 input = input |> parse |> solve |> string_of_int
let part2 input = input |> parse |> solve_2 |> string_of_int
