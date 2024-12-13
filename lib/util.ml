let ( >> ) f g x = g (f x)
let to_charlist string = List.init (String.length string) (String.get string)

(*Horrible performance but who cares*)
let drop_last ls = ls |> List.rev |> List.tl |> List.rev
let range start stop = List.init (stop - start) (fun i -> start + i)

let coords (matrix : 'a list list) : (int * int) list =
  List.mapi (fun row_idx row -> List.mapi (fun col_idx _ -> (row_idx, col_idx)) row) matrix
  |> List.concat

let sum coll = List.fold_left ( + ) 0 coll

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let read_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let neighbours (x, y) : (int * int) list =
  [
    (x + 1, y);
    (x - 1, y);
    (x, y + 1);
    (x, y - 1);
    (x + 1, y + 1);
    (x + 1, y - 1);
    (x - 1, y + 1);
    (x - 1, y - 1);
  ]
  |> List.filter (fun (x, y) -> x >= 0 && y >= 0)

let safe_get (x, y) matrix : 'a option =
  List.nth_opt matrix y |> Option.map (fun row -> List.nth_opt row x) |> Option.join

let array_nth_opt arr i = if i < 0 || i >= Array.length arr then None else Some arr.(i)

let safe_get_a (x, y) matrix : 'a option =
  array_nth_opt matrix y |> Option.map (fun row -> array_nth_opt row x) |> Option.join

let pretty_print_string_list (lst : int list) : string =
  let quoted = List.map (fun s -> "\"" ^ string_of_int s ^ "\"") lst in
  "[" ^ String.concat "; " quoted ^ "]"

(*Clojure's re-seq *)
let re_seq regex str =
  let re = Str.regexp regex in
  let rec aux pos =
    try
      let _ = Str.search_forward re str pos in
      let match_str = Str.matched_string str in
      match_str :: aux (Str.match_end ())
    with Not_found -> []
  in
  aux 0

(*let last list : 'a option =*)
(*  let rec go rem = match rem with head :: [] -> Some head | _ :: tail -> go tail | [] -> None in*)
(*  go list*)

let last array : 'a option = array_nth_opt array (Array.length array - 1)

let tuple_to_string (a, b) = Printf.sprintf "(%d, %d)" a b
