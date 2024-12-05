module StringPair = struct
  type t = string * string

  let compare (a1, b1) (a2, b2) =
      match String.compare a1 a2 with
          | 0 -> String.compare b1 b2
          | cmp -> cmp
end

module StringPairSet = Set.Make (StringPair)

type parsed = { orders : StringPairSet.t; updates : string list list }

let parse parsed : parsed =
    let lines = Base.String.split_lines parsed in
    let first_part =
        Base.List.take_while lines ~f:(fun line -> Base.String.is_empty line |> Bool.not)
    in
    let second_part = Base.List.drop lines (List.length first_part + 1) in
    let orders =
        List.tl first_part
        |> List.map (fun line ->
               let split = String.split_on_char '|' line in
                   (List.nth split 0, List.nth split 1))
        |> List.to_seq |> StringPairSet.of_seq
    in
    let updates = List.map (fun line -> String.split_on_char ',' line) second_part in

    { orders; updates }

let validate update parsed : bool =
    let rec go rem : bool =
        match rem with
            | head :: next :: tail ->
                let mapping_exists = StringPairSet.mem (head, next) parsed.orders in
                    if mapping_exists then
                      go (next :: tail)
                    else
                      false
            | _ -> true
    in
        go update

let get_middle list =
    let len = List.length list in
    let mid = float_of_int len /. 2.0 |> Float.round |> int_of_float in
        List.nth list (mid - 1)

let pair_up elem list : (string * string) list =
    let rec go rem acc =
        match rem with
            | head :: tail -> go tail ((elem, head) :: acc)
            | [] -> acc
    in
        go list []

let match_count pairs order : int =
    List.filter (fun pair -> StringPairSet.mem pair order) pairs |> List.length

let fix_line line parsed : string list =
    List.map
      (fun elem ->
        let matches = match_count (pair_up elem line) parsed.orders in
            (elem, matches))
      line
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map fst

let part1 input =
    let parsed = parse input in
        List.filter (fun update -> validate update parsed) parsed.updates
        |> List.map (fun update -> get_middle update |> int_of_string)
        |> Util.sum |> string_of_int

let part2 input =
    let parsed = parse input in
        List.filter (fun update -> validate update parsed |> Bool.not) parsed.updates
        |> List.map (fun update -> fix_line update parsed)
        |> List.map (fun update -> get_middle update |> int_of_string)
        |> Util.sum |> string_of_int
