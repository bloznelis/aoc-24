module IntTuple = struct
  type t = int * int

  let equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
  let hash (a, b) = Hashtbl.hash (a, b)
end

module IntTupleHashtbl = Hashtbl.Make (IntTuple)

let even_digits stone : bool = string_of_int stone |> String.length |> fun x -> x land 1 = 0

let split stone : int list =
  let str = string_of_int stone in
  let mid = String.length str / 2 in
  let first = String.sub str 0 mid |> int_of_string in
  let second = String.sub str mid mid |> int_of_string in
  [ first; second ]

let evolve stone : int list =
  match stone with
  | 0 -> [ 1 ]
  | stone when even_digits stone -> split stone
  | stone -> [ stone * 2024 ]

let table = IntTupleHashtbl.create 1000

let rec blink cnt limit stone =
  let maybe_cached = IntTupleHashtbl.find_opt table (stone, cnt) in
  match maybe_cached with
  | Some cached -> cached
  | None ->
      let result =
        if cnt == limit then 1
        else evolve stone |> List.fold_left (fun acc stone -> acc + blink (cnt + 1) limit stone) 0
      in
      IntTupleHashtbl.replace table (stone, cnt) result;
      result

let parse input : int list = input |> Util.re_seq "[0-9]+" |> List.map int_of_string

let solve input blink_limit =
  IntTupleHashtbl.clear table;
  input |> parse |> List.map (blink 0 blink_limit) |> Util.sum |> string_of_int

let part1 input = solve input 25
let part2 input = solve input 75
