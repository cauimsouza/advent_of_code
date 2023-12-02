let read_lines () =
  let in_ch = open_in "input_p1.txt" in
  let read_line () =
    try Some (input_line in_ch) with End_of_file -> None in
  let rec f acc =
    match read_line () with
    | Some l -> f (l :: acc)
    | None -> List.rev acc
  in f []

let get_num_digit str i =
  let c = String.get str i in
  match c with
  | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None

let get_alpha_digit str i =
  let digits = [
    ("zero", 0);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9)]
  in
  let matches_digit (dig_str, _) =
    i + String.length dig_str <= String.length str &&
    String.sub str i (String.length dig_str) = dig_str
  in
  try
    let (_, d) = List.find matches_digit digits in Some d
  with Not_found -> None

let get_alphanum_digit str i =
  match get_num_digit str i with
  | Some d -> Some d
  | None -> get_alpha_digit str i

let first_digit getter str =
  let rec f str i =
    match getter str i with
    | None -> f str (i + 1)
    | Some d -> d 
  in
  f str 0

let last_digit getter str =
  let rec f str i =
    match getter str i with 
    | None -> f str (i - 1)
    | Some d -> d
  in 
  f str (String.length str - 1)

let calibration_value getter str =
  let first = first_digit getter str in
  let last = last_digit getter str in
  first * 10 + last

let () = 
  let lines = read_lines () in
  let ans1 = List.fold_left (fun acc line -> acc + calibration_value get_num_digit line) 0 lines in
  let ans2 = List.fold_left (fun acc line -> acc + calibration_value get_alphanum_digit line) 0 lines in
  Printf.printf "Answer to part 1 is %d\nAnswer to part 2 is %d\n" ans1 ans2
