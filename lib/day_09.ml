(* I'm not even sorry *)

type free = { range_start : int; range_end : int }
type occupied = { file_id : int; range_start : int; range_end : int }
type space = Occupied of occupied | Free of free

let parse (input : string) : space list =
  let ints =
    input |> String.trim |> String.to_seq
    |> Seq.map (fun char -> Base.Char.get_digit char |> Option.get)
    |> List.of_seq
  in
  let rec go rem file_flag file_idx idx acc =
    match rem with
    | head :: tail ->
        if file_flag then
          go tail false (file_idx + 1) (idx + head)
            (Occupied { file_id = file_idx; range_start = idx; range_end = idx + head } :: acc)
        else if head = 0 then go tail true file_idx idx acc
        else
          go tail true file_idx (idx + head)
            (Free { range_start = idx; range_end = idx + head } :: acc)
    | [] -> acc
  in
  go ints true 0 0 [] |> List.rev

let contract (space : space) : space =
  match space with
  | Occupied o -> Occupied { o with range_end = o.range_end - 1 }
  | Free f -> Free { f with range_end = f.range_end - 1 }

let expand (space : space) : space =
  match space with
  | Occupied o -> Occupied { o with range_end = o.range_end + 1 }
  | Free f -> Free { f with range_end = f.range_end + 1 }

let contract_last spaces : space list =
  let rec go rem acc =
    match rem with
    | last :: [] -> contract last :: acc |> List.rev
    | head :: tail -> go tail (head :: acc)
    | [] -> failwith "rip"
  in
  go spaces []

let add_to_front file_id (spaces : space list) : space list =
  let rec go (rem : space list) (acc : space list) =
    match rem with
    | (Occupied occupied as whole_occupied) :: Free free :: tail when occupied.file_id = file_id ->
        let new_occupied = expand whole_occupied in
        let new_free = { free with range_start = free.range_start + 1 } in
        let new_acc =
          if new_free.range_end = new_free.range_start then new_occupied :: acc
          else Free new_free :: new_occupied :: acc
        in
        List.rev_append new_acc tail
    | Free free :: tail ->
        let new_occupied =
          Occupied { file_id; range_start = free.range_start; range_end = free.range_start + 1 }
        in
        let new_free = { free with range_start = free.range_start + 1 } in
        let new_acc =
          if new_free.range_end = new_free.range_start then new_occupied :: acc
          else Free new_free :: new_occupied :: acc
        in
        List.rev_append new_acc tail
    | head :: tail -> go tail (head :: acc)
    | [] -> failwith "rip"
  in
  go spaces []

let compact_one_block (spaces : space list) : space list =
  let last = Base.List.last spaces in
  match last with
  | Some (Occupied occupied) ->
      if occupied.range_end = occupied.range_start then Util.drop_last spaces
      else contract_last spaces |> add_to_front occupied.file_id
  | Some (Free _) -> Util.drop_last spaces
  | None -> failwith "rip"

let range_sum range_start range_end =
  let rec aux acc i = if i < range_start then acc else aux (acc + i) (i - 1) in
  aux 0 range_end

let defrag (spaces : space list) : space list =
  let rec go (current_spaces : space list) =
    let has_free_space =
      List.find_opt
        (fun space -> match space with Occupied _ -> false | Free _ -> true)
        current_spaces
      |> Option.is_some
    in
    if has_free_space then go (compact_one_block current_spaces) else current_spaces
  in
  go spaces

let final_sum (spaces : space list) : int =
  let rec go rem acc =
    match rem with
    | Occupied occupied :: tail ->
        go tail acc + (range_sum occupied.range_start (occupied.range_end - 1) * occupied.file_id)
    | Free _ :: tail -> go tail acc
    | [] -> acc
  in
  go spaces 0

let merge_free_space (spaces : space list) =
  let rec go rem acc =
    match rem with
    | Free free :: Free free2 :: tail ->
        go tail (Free { range_start = free.range_start; range_end = free2.range_end } :: acc)
    | other :: tail -> go tail (other :: acc)
    | [] -> List.rev acc
  in
  go spaces []

let try_move_file (file : occupied) (spaces : space list) : space list =
  let file_size = file.range_end - file.range_start in
  let rec go rem acc =
    match rem with
    | (Occupied occupied as oc) :: tail ->
        if occupied.file_id == file.file_id then spaces else go tail (oc :: acc)
    | Free free :: tail ->
        let free_space = free.range_end - free.range_start in
        if free_space >= file_size then
          let new_occupied =
            {
              file_id = file.file_id;
              range_start = free.range_start;
              range_end = free.range_start + file_size;
            }
          in
          let new_tail =
            tail
            |> List.map (fun a ->
                   match a with
                   | Occupied oc ->
                       if oc.file_id = file.file_id then
                         Free { range_start = oc.range_start; range_end = oc.range_end }
                       else Occupied oc
                   | Free _ as free -> free)
          in
          let new_free = { free with range_start = new_occupied.range_end } in
          let new_acc =
            if new_free.range_end = new_free.range_start then Occupied new_occupied :: acc
            else Free new_free :: Occupied new_occupied :: acc
          in
          List.rev_append new_acc new_tail
        else go tail (Free free :: acc)
    | [] -> acc |> List.rev
  in
  go spaces []

let defrag_2 (spaces : space list) : space list =
  let rec go (rem : occupied list) latest =
    match rem with
    | head :: tail -> go tail (try_move_file head latest |> merge_free_space)
    | [] -> latest
  in

  go
    (spaces
    |> List.filter_map (fun a -> match a with Occupied o -> Some o | Free _ -> None)
    |> List.sort (fun a b -> compare b.file_id a.file_id))
    spaces

let part1 input = input |> parse |> defrag |> final_sum |> string_of_int
let part2 input = input |> parse |> defrag_2 |> final_sum |> string_of_int
