let read_lines () =
  let ch = open_in "input_p2.txt" in
  let get_line () =
    try Some (input_line ch) with End_of_file -> None
  in 
  let rec f acc =
    match get_line () with 
    | None -> List.rev acc
    | Some line -> f (line :: acc)
  in 
  f []

type draw = {
  red : int;
  green : int;
  blue : int;
}

type game = {
  id : int;
  draws : draw list;
}

let init_draw () = { red = 0; green = 0; blue = 0 }

let line_to_game str =
  let parse_draws draws =
    let nc_strs = List.map String.trim (String.split_on_char ',' draws) in
    let ncs = List.map (fun s -> let nc = String.split_on_char ' ' s in (int_of_string (List.hd nc), List.nth nc 1)) nc_strs in
    let update_draw draw (num, col) = match col with
    | "red" -> { draw with red = num }
    | "green" -> { draw with green = num }
    | "blue" -> { draw with blue = num}
    | _ -> assert false
    in
    List.fold_left update_draw (init_draw ()) ncs
  in
  let parse_draws_line line =
    let draws = List.map String.trim (String.split_on_char ';' line) in
    List.map parse_draws draws
  in
  let re = Str.regexp "Game \\([0-9]+\\):\\(.*$\\)" in 
  let _ = Str.string_match re str 0 in
  let id_str = Str.matched_group 1 str in
  let draws_str = Str.matched_group 2 str in
  {
    id = int_of_string id_str;
    draws = parse_draws_line draws_str;
  }

let is_possible game =
  let is_possible_draw draw =
    draw.red <= 12 && draw.green <= 13 && draw.blue <= 14
  in
  List.for_all is_possible_draw game.draws

let power game =
  let max_draw a b =
    {
      red = max a.red b.red;
      green = max a.green b.green;
      blue = max a.blue b.blue;
    }
  in
  let { red; green; blue } = List.fold_left max_draw (init_draw ()) game.draws in
  red * green * blue

let () =
  let lines = read_lines() in
  let games = List.map line_to_game lines in
  let ans1 = List.fold_left (fun acc game -> if is_possible game then game.id + acc else acc) 0 games in 
  let ans2 = List.fold_left (fun acc game -> acc + power game) 0 games in
  Printf.printf "Answer to part 1 is %d\nAnswer to part 2 is %d\n" ans1 ans2
