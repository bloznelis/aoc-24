type direction = Top | Right | Bot | Left
type guard_result = Loop | Outside of int

module Coords = struct
  type t = int * int

  let compare = compare
end

module DirectionalCoords = struct
  type t = { pos : int * int; dir : direction }

  let compare t1 t2 = compare (t1.pos, t1.dir) (t2.pos, t2.dir)
end

module DirectionalCoordsSet = Set.Make (DirectionalCoords)
module CoordsSet = Set.Make (Coords)

type guard = { pos : int * int; dir : direction; visited : DirectionalCoordsSet.t }

let parse input = Base.String.split_lines input |> Array.of_list |> Array.map Base.String.to_array

let find_guard matrix =
  let pos =
    Array.find_mapi
      (fun y row ->
        Array.find_mapi (fun x char -> match char with '^' -> Some (x, y) | _ -> None) row)
      matrix
    |> Option.get
  in
  { pos; dir = Top; visited = DirectionalCoordsSet.empty }

let next_cell direction (x, y) =
  match direction with
  | Top -> (x, y - 1)
  | Right -> (x + 1, y)
  | Bot -> (x, y + 1)
  | Left -> (x - 1, y)

let turn_right direction =
  match direction with Top -> Right | Right -> Bot | Bot -> Left | Left -> Top

type step_result = Loop | OutOfBounds | StepAvailable of guard

let single_step guard matrix =
  let in_loop = DirectionalCoordsSet.mem { pos = guard.pos; dir = guard.dir } guard.visited in
  if in_loop then Loop
  else
    let next_pos = next_cell guard.dir guard.pos in
    match Util.safe_get_a next_pos matrix with
    | Some '#' -> StepAvailable { guard with dir = turn_right guard.dir }
    | Some '.' | Some '^' ->
        StepAvailable
          {
            pos = next_pos;
            dir = guard.dir;
            visited = guard.visited |> DirectionalCoordsSet.add { pos = guard.pos; dir = guard.dir };
          }
    | _ -> OutOfBounds

let to_coords (dir : DirectionalCoords.t) : Coords.t = dir.pos

let move_guard_along_path guard matrix : guard_result =
  let rec go guard =
    let step_res = single_step guard matrix in
    match step_res with
    | OutOfBounds ->
        let outside =
          DirectionalCoordsSet.elements guard.visited
          |> List.map to_coords |> CoordsSet.of_list |> CoordsSet.to_list |> List.length
        in
        Outside outside
    | StepAvailable guard -> go guard
    | Loop -> Loop
  in
  go guard

let add_blocker (x, y) matrix =
  match Util.safe_get_a (x, y) matrix with
  | Some _ ->
      let new_matrix = Array.map Array.copy matrix in
      new_matrix.(y).(x) <- '#';
      new_matrix
  | None -> matrix

let find_blockers initial_guard matrix : CoordsSet.t =
  let rec go guard obstructions =
    let blocker_at = next_cell guard.dir guard.pos in
    let guard_result = add_blocker blocker_at matrix |> move_guard_along_path initial_guard in
    match (guard_result, single_step guard matrix) with
    | Outside _, StepAvailable next_guard -> go next_guard obstructions
    | Loop, StepAvailable next_guard -> go next_guard (CoordsSet.add blocker_at obstructions)
    | _, OutOfBounds -> obstructions
    | _, Loop -> failwith "unexpected loop"
  in
  go initial_guard CoordsSet.empty

let part1 input =
  let matrix = parse input in
  let guard = find_guard matrix in
  let res = move_guard_along_path guard matrix in
  match res with Outside visits -> visits + 1 |> string_of_int | Loop -> "loop!"

let part2 input =
  let matrix = parse input in
  let guard = find_guard matrix in
  find_blockers guard matrix |> CoordsSet.to_list |> List.length |> string_of_int
