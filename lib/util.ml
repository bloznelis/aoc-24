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

