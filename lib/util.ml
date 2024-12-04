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
      (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1); (x + 1, y + 1); (x + 1, y - 1);
      (x - 1, y + 1); (x - 1, y - 1);
    ]
    |> List.filter (fun (x, y) -> x >= 0 && y >= 0)

let safe_get (x, y) matrix : 'a option =
    List.nth_opt matrix y |> Option.map (fun row -> List.nth_opt row x) |> Option.join
