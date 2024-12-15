type direction = Up | Right | Down | Left [@@deriving show]

type big_box = {
  first_part : int * int;
  first_char : char;
  second_part : int * int;
  second_char : char;
}
[@@deriving show]

type obj = BigBox of big_box | Free | Wall
type boxes_to_move = Blocked | Move of big_box list

let set_matrix a matrix (x, y) = matrix.(y).(x) <- a

let parse_map input : char array array =
  Base.String.split_lines input
  |> Base.List.take_while ~f:(fun line -> Base.String.is_empty line |> Bool.not)
  |> List.map (fun line -> Base.String.to_array line)
  |> Array.of_list

let parse_dir input : direction list =
  Base.String.split_lines input
  |> Base.List.drop_while ~f:(fun line -> Base.String.is_empty line |> Bool.not)
  |> List.tl |> String.concat "" |> Base.String.to_list
  |> List.map (fun char ->
         match char with
         | '^' -> Up
         | '>' -> Right
         | 'v' -> Down
         | '<' -> Left
         | _ -> failwith "bad input")

let find_robot matrix =
  Array.find_mapi
    (fun y line ->
      Array.find_mapi (fun x char -> match char with '@' -> Some (x, y) | _ -> None) line)
    matrix
  |> Option.get

let gps_sum matrix =
  Array.mapi
    (fun y line ->
      Array.mapi (fun x char -> match char with 'O' -> x + (y * 100) | _ -> 0) line
      |> Array.to_list |> Util.sum)
    matrix
  |> Array.to_list |> Util.sum

let gps_sum_2 matrix =
  Array.mapi
    (fun y line ->
      Array.mapi (fun x char -> match char with '[' -> x + (y * 100) | _ -> 0) line
      |> Array.to_list |> Util.sum)
    matrix
  |> Array.to_list |> Util.sum

let exapand_line line =
  let rec go rem acc =
    match rem with
    | '.' :: tail -> go tail ('.' :: '.' :: acc)
    | '#' :: tail -> go tail ('#' :: '#' :: acc)
    | 'O' :: tail -> go tail (']' :: '[' :: acc)
    | '@' :: tail -> go tail ('.' :: '@' :: acc)
    | [] -> List.rev acc
    | _ -> failwith "bad input"
  in
  go line []

let expand_map original_map : char array array =
  let dimy = Array.length original_map in
  let dimx = original_map.(0) |> Array.length |> fun x -> x * 2 in
  let copy = Array.make_matrix dimy dimx '.' in
  Array.iteri
    (fun y line -> copy.(y) <- Array.to_list line |> exapand_line |> Array.of_list)
    original_map;
  copy

let offset (x, y) direction =
  match direction with
  | Up -> (x, y - 1)
  | Right -> (x + 1, y)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)

let rec move_boxes box_list direction matrix =
  let leading_box = List.hd box_list in
  let next_box = offset leading_box direction in
  match Util.safe_get_a next_box matrix with
  | Some '.' ->
      List.iter (fun box_pos -> offset box_pos direction |> set_matrix 'O' matrix) box_list;
      true
  | Some 'O' -> move_boxes (next_box :: box_list) direction matrix
  | Some '#' -> false
  | _ -> failwith "not expected"

let move_robot pos next_pos matrix =
  set_matrix '.' matrix pos;
  set_matrix '@' matrix next_pos;
  next_pos

let move robot_pos direction matrix : int * int =
  let next_pos = offset robot_pos direction in
  match Util.safe_get_a next_pos matrix with
  | Some '.' -> move_robot robot_pos next_pos matrix
  | Some 'O' ->
      if move_boxes [ next_pos ] direction matrix then move_robot robot_pos next_pos matrix
      else robot_pos
  | Some '#' -> robot_pos
  | Some _ -> failwith "this should not happen"
  | None -> robot_pos

let rec move_all robot_pos directions matrix =
  match directions with
  | head :: tail ->
      let new_robot_pos = move robot_pos head matrix in
      move_all new_robot_pos tail matrix
  | [] -> ()

let get_obj (x, y) matrix : obj =
  match Util.safe_get_a (x, y) matrix with
  | Some '[' ->
      BigBox { first_part = (x, y); first_char = '['; second_part = (x + 1, y); second_char = ']' }
  | Some ']' ->
      BigBox { first_part = (x, y); first_char = ']'; second_part = (x - 1, y); second_char = '[' }
  | Some '#' -> Wall
  | Some '.' -> Free
  | _ -> failwith "unknown"

let move_big_box big_box direction matrix =
  big_box.first_part |> set_matrix '.' matrix;
  big_box.second_part |> set_matrix '.' matrix;
  offset big_box.first_part direction |> set_matrix big_box.first_char matrix;
  offset big_box.second_part direction |> set_matrix big_box.second_char matrix

let rec collect_boxes_to_move box_list direction matrix =
  let leading_box = List.hd box_list in
  match direction with
  | Right | Left -> (
      let box_is_next = get_obj (offset leading_box.second_part direction) matrix in
      match box_is_next with
      | Wall -> Blocked
      | Free -> Move box_list
      | BigBox box -> collect_boxes_to_move (box :: box_list) direction matrix)
  | Up | Down -> (
      let next_box_1 = get_obj (offset leading_box.first_part direction) matrix in
      let next_box_2 = get_obj (offset leading_box.second_part direction) matrix in
      match (next_box_1, next_box_2) with
      | Wall, _ -> Blocked
      | _, Wall -> Blocked
      | Free, Free -> Move box_list
      | BigBox box1, BigBox box2 -> (
          let collected1 = collect_boxes_to_move (box1 :: box_list) direction matrix in
          let collected2 = collect_boxes_to_move (box2 :: box_list) direction matrix in
          match (collected1, collected2) with
          | Blocked, _ -> Blocked
          | _, Blocked -> Blocked
          | Move boxes1, Move boxes2 -> Move (List.append (List.append box_list boxes1) boxes2))
      | BigBox box1, Free -> (
          let collected = collect_boxes_to_move (box1 :: box_list) direction matrix in
          match collected with
          | Blocked -> Blocked
          | Move boxes -> Move (List.append boxes box_list))
      | Free, BigBox box2 -> (
          let collected = collect_boxes_to_move (box2 :: box_list) direction matrix in
          match collected with
          | Blocked -> Blocked
          | Move boxes -> Move (List.append boxes box_list)))

let sort_boxes box_list direction =
  match direction with
  | Up -> List.sort (fun a b -> compare (a.first_part |> snd) (b.first_part |> snd)) box_list
  | Down -> List.sort (fun a b -> compare (b.first_part |> snd) (a.first_part |> snd)) box_list
  | Right -> List.sort (fun a b -> compare (b.first_part |> fst) (a.first_part |> fst)) box_list
  | Left -> List.sort (fun a b -> compare (a.first_part |> fst) (b.first_part |> fst)) box_list

let move_big_boxes box_list direction matrix =
  sort_boxes box_list direction |> List.iter (fun box -> move_big_box box direction matrix)

let move_2 robot_pos direction matrix : int * int =
  let next_pos = offset robot_pos direction in
  match Util.safe_get_a next_pos matrix with
  | Some '.' -> move_robot robot_pos next_pos matrix
  | Some '#' -> robot_pos
  | Some ']' | Some '[' -> (
      let big_box = get_obj next_pos matrix in
      let collect_res =
        match big_box with
        | BigBox box -> collect_boxes_to_move [ box ] direction matrix
        | _ -> failwith "expected big box"
      in
      match collect_res with
      | Move boxes ->
          move_big_boxes boxes direction matrix;
          move_robot robot_pos next_pos matrix
      | Blocked -> robot_pos)
  | Some _ -> failwith "this should not happen"
  | None -> robot_pos

let rec move_all_2 robot_pos directions matrix =
  match directions with
  | head :: tail ->
      let new_robot_pos = move_2 robot_pos head matrix in
      move_all_2 new_robot_pos tail matrix
  | [] -> ()

let part1 input =
  let matrix = input |> parse_map in
  let directions = input |> parse_dir in
  let robot_pos = find_robot matrix in
  move_all robot_pos directions matrix;
  gps_sum matrix |> string_of_int

let part2 input =
  let matrix = input |> parse_map |> expand_map in
  let directions = input |> parse_dir in
  let robot_pos = find_robot matrix in
  move_all_2 robot_pos directions matrix;
  gps_sum_2 matrix |> string_of_int
