type plot_positions = (int * int, unit) Hashtbl.t
type plot = { char : char; positions : plot_positions }
type direction = Left | Top | Right | Bot [@@deriving show]
type side = { direction : direction; members : (int * int) list } [@@deriving show]

let parse_map input : char array array =
  Base.String.split_lines input |> List.map (fun line -> Base.String.to_array line) |> Array.of_list

let explore_plot start_pos plot_char matrix =
  let acc = Hashtbl.create 100 in
  let rec go position =
    match Util.safe_get_a position matrix with
    | Some found_plot_char when found_plot_char = plot_char -> (
        match Hashtbl.find_opt acc position with
        | Some _ -> ()
        | None ->
            let _ = Hashtbl.replace acc position () in
            Util.neighbours_4 position |> List.iter (fun neighbour -> go neighbour))
    | _ -> ()
  in

  go start_pos;
  acc

let is_already_registered pos plots =
  List.find_opt (fun plot -> Hashtbl.mem plot.positions pos) plots |> Option.is_some

let collect_all_plots matrix =
  let all_positions =
    Array.mapi (fun y arr -> Array.mapi (fun x char -> ((x, y), char)) arr |> Array.to_list) matrix
    |> Array.to_list |> List.flatten
  in
  let rec go rem acc =
    match rem with
    | (pos, char) :: tail ->
        if is_already_registered pos acc then go tail acc
        else go tail ({ char; positions = explore_plot pos char matrix } :: acc)
    | [] -> acc
  in
  go all_positions []

let get_possible_sides char (x, y) matrix =
  let boundary_at pos side =
    match Util.safe_get_a pos matrix with
    | Some c when c != char -> Some side
    | None -> Some side
    | _ -> None
  in
  [
    boundary_at (x + 1, y) Right;
    boundary_at (x - 1, y) Left;
    boundary_at (x, y + 1) Bot;
    boundary_at (x, y - 1) Top;
  ]
  |> Base.List.filter_opt

let collect_shell plot matrix =
  Hashtbl.to_seq_keys plot.positions
  |> Seq.map (fun pos -> get_possible_sides plot.char pos matrix |> List.map (fun _ -> pos))
  |> List.of_seq |> List.flatten

let plot_perimeter plot matrix = collect_shell plot matrix |> List.length

let move (x, y) direction =
  match direction with
  | Top -> (x, y - 1)
  | Right -> (x + 1, y)
  | Bot -> (x, y + 1)
  | Left -> (x - 1, y)

let collect_single_side pos direction char matrix =
  let rec go move_direction current_pos acc =
    let neigh_pos = move current_pos move_direction in
    match Util.safe_get_a neigh_pos matrix with
    | Some neigh_char when neigh_char = char ->
        let is_good =
          get_possible_sides char neigh_pos matrix
          |> List.find_opt (fun x -> x = direction)
          |> Option.is_some
        in
        if is_good then go move_direction neigh_pos (neigh_pos :: acc) else acc
    | _ -> acc
  in
  let explore dir1 dir2 =
    { direction; members = List.append (go dir1 pos [ pos ]) (go dir2 pos [ pos ]) }
  in
  match direction with
  | Top | Bot -> explore Left Right
  | Right | Left -> explore Top Bot

let is_part_of_side pos direction sides =
  List.find_opt
    (fun side ->
      side.direction = direction && List.find_opt (fun x -> x = pos) side.members |> Option.is_some)
    sides
  |> Option.is_some

let collect_sides plot matrix =
  let shell = collect_shell plot matrix in
  let rec go rem acc =
    match rem with
    | pos :: tail ->
        let possible_sides = get_possible_sides plot.char pos matrix in
        let sides =
          List.map
            (fun possible_side ->
              if is_part_of_side pos possible_side acc then None
              else Some (collect_single_side pos possible_side plot.char matrix))
            possible_sides
          |> Base.List.filter_opt
        in
        go tail (List.append acc sides)
    | [] -> acc
  in
  go shell [] |> List.length

let compute_price matrix plots perimeter_fn =
  List.map (fun plot -> Hashtbl.length plot.positions * perimeter_fn plot matrix) plots |> Util.sum

let compute_price_1 matrix plots = compute_price matrix plots plot_perimeter
let compute_price_2 matrix plots = compute_price matrix plots collect_sides

let solve input price_fn =
  let matrix = parse_map input in
  collect_all_plots matrix |> price_fn matrix |> string_of_int

let part1 input = solve input compute_price_1
let part2 input = solve input compute_price_2
