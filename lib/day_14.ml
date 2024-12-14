type robot = { px : int; py : int; vx : int; vy : int } [@@deriving show]
type quadrants = { first : int; second : int; third : int; fourth : int } [@@deriving show]

let move robot (x_limit, y_limit) =
  let new_x = robot.px + robot.vx in
  let new_x =
    if new_x >= x_limit then new_x - x_limit else if new_x < 0 then x_limit + new_x else new_x
  in
  let new_y = robot.py + robot.vy in
  let new_y =
    if new_y >= y_limit then new_y - y_limit else if new_y < 0 then y_limit + new_y else new_y
  in
  { robot with px = new_x; py = new_y }

let rec move_n_times n robot limits =
  if n = 0 then robot else move_n_times (n - 1) (move robot limits) limits

let parse input =
  Base.String.split_lines input
  |> List.map (fun line ->
         match Util.re_seq "-?[0-9]+" line |> List.map int_of_string with
         | [ px; py; vx; vy ] -> { px; py; vx; vy }
         | _ -> failwith "bad input")

let product quadrants =
  print_endline (show_quadrants quadrants);
  quadrants.first * quadrants.second * quadrants.third * quadrants.fourth

let add robot (mid_x, mid_y) quadrants =
  let x, y = (robot.px, robot.py) in
  if x < mid_x && y < mid_y then { quadrants with first = quadrants.first + 1 }
  else if x > mid_x && y < mid_y then { quadrants with second = quadrants.second + 1 }
  else if x < mid_x && y > mid_y then { quadrants with third = quadrants.third + 1 }
  else if x > mid_x && y > mid_y then { quadrants with fourth = quadrants.fourth + 1 }
  else quadrants

let move_all robots =
  let ((x_limit, y_limit) as limits) = (101, 103) in
  List.map (fun robot -> move_n_times 100 robot limits) robots
  |> List.fold_left
       (fun acc elem -> add elem (x_limit / 2, y_limit / 2) acc)
       { first = 0; second = 0; third = 0; fourth = 0 }
  |> product

let has_adjacent_robots robot matrix =
  Util.neighbours (robot.px, robot.py)
  |> List.map (fun (x, y) -> Util.safe_get_a (x, y) matrix)
  |> List.filter (fun elem -> match elem with Some e when e = '#' -> true | _ -> false)
  |> List.is_empty |> Bool.not

let look_for_drawing (x_limit, y_limit) iteration robots =
  let array = Array.init y_limit (fun _ -> Array.make x_limit '.') in
  List.iter (fun robot -> array.(robot.py).(robot.px) <- '#') robots;
  let is_drawing =
    List.filter (fun robot -> has_adjacent_robots robot array) robots |> List.length > 300
  in

  if is_drawing then (
    print_newline ();
    let _ = print_endline ("@@@@@@ Iteration:" ^ string_of_int iteration ^ " @@@@@@") in
    Array.iter (fun line -> print_endline (Base.String.of_array line)) array);

  is_drawing

let find_easter_egg robots =
  let limits = (101, 103) in
  let rec go cnt bots =
    let new_robots = List.map (fun robot -> move robot limits) bots in
    let found_it = look_for_drawing limits cnt new_robots in
    if found_it then cnt else go (cnt + 1) new_robots
  in
  go 1 robots

let part1 input = input |> parse |> move_all |> string_of_int
let part2 input = input |> parse |> find_easter_egg |> string_of_int
