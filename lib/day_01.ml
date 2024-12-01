let parse input =
    String.trim input |> Base.String.split_lines
    |> List.map (fun line -> Str.split (Str.regexp " +") line |> List.map int_of_string)
    |> Util.transpose

module FreqMap = Map.Make (Int)

let frequencies (coll : int list) : int FreqMap.t =
    List.fold_left
      (fun acc elem ->
        FreqMap.update elem
          (fun existing ->
            match existing with
                | None -> Some 1
                | Some count -> Some (count + 1))
          acc)
      FreqMap.empty coll

let calc_2 (parsed : int list list) =
    let freqs = frequencies (List.nth parsed 1) in
        List.fold_left
          (fun acc elem -> acc + (elem * Option.value ~default:0 (FreqMap.find_opt elem freqs)))
          0 (List.nth parsed 0)

let calc_1 (parsed : int list list) =
    let sort coll = List.sort compare coll in
    let sorted = List.map sort parsed in
        List.fold_left2
          (fun acc a b -> acc + Int.abs (a - b))
          0 (List.nth sorted 0) (List.nth sorted 1)

let part1 input_text = input_text |> parse |> calc_1 |> string_of_int
let part2 input_text = input_text |> parse |> calc_2 |> string_of_int
