let print_answer label answer = String.cat label answer |> print_endline

let () =
    (*Aoc24.Util.read_file "input/day_01.txt" |> Aoc24.Day_01.part1 |> print_answer "Day 1 Part 1: ";*)
    (*Aoc24.Util.read_file "input/day_01.txt" |> Aoc24.Day_01.part2 |> print_answer "Day 1 Part 2: ";*)
    (*Aoc24.Util.read_file "input/day_02.txt" |> Aoc24.Day_02.part1 |> print_answer "Day 2 Part 1: ";*)
    (*Aoc24.Util.read_file "input/day_02.txt" |> Aoc24.Day_02.part2 |> print_answer "Day 2 Part 2: ";*)
    (*Aoc24.Util.read_file "input/day_03.txt" |> Aoc24.Day_03.part1 |> print_answer "Day 3 Part 1: ";*)
    (*Aoc24.Util.read_file "input/day_03.txt" |> Aoc24.Day_03.part2 |> print_answer "Day 3 Part 2: "*)
    Aoc24.Util.read_file "input/day_04.txt" |> Aoc24.Day_04.part1 |> print_answer "Day 4 Part 1: ";
    Aoc24.Util.read_file "input/day_04.txt" |> Aoc24.Day_04.part2 |> print_answer "Day 4 Part 2: "
