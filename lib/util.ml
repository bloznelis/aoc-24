let ( >> ) f g x = g (f x)

(*Horrible performance but who cares*)
let drop_last ls = ls |> List.rev |> List.tl |> List.rev

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
