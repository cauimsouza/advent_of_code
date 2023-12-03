let read_lines () =
  let ch = open_in "input_p3.txt" in
  let get_line () =
    try Some (input_line ch) with End_of_file -> None
  in 
  let rec f acc =
    match get_line () with 
    | None -> List.rev acc
    | Some line -> f (line :: acc)
  in 
  f []

let build_matrix lines =
  let str_to_array str =
    Array.init (String.length str) (String.get str)
  in
  let nrows = List.length lines in 
  Array.init nrows (fun i -> str_to_array (List.nth lines i))

let is_digit = function 
  | '0' .. '9' -> true 
  | _ -> false

let is_symbol = function
  | '0' ..'9' | '.' -> false 
  | _ -> true

let is_adjacent_to_symbol matrix i j = 
  let nrows = Array.length matrix in
  let ncols = Array.length matrix.(i) in
  let adjacent = ref false in 
  let j = ref j in 
  while not !adjacent && !j < ncols && is_digit matrix.(i).(!j) do
    let ij = [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)] in
    let f (di, dj) =
      let i = i + di in 
      let j = !j + dj in 
      adjacent := !adjacent || (i >= 0 && i < nrows && j >= 0 && j < ncols && is_symbol matrix.(i).(j))
    in
    List.iter f ij; j := !j + 1
  done; !adjacent

let is_part_number matrix i j = 
  is_digit matrix.(i).(j) &&
  (j = 0 || not @@ is_digit matrix.(i).(j - 1)) &&
  is_adjacent_to_symbol matrix i j

let get_number matrix i j =
  let int_of_char c = int_of_char c - int_of_char '0' in 
  let ncols = Array.length matrix.(i) in 
  let n = ref 0 in 
  let j = ref j in 
  while !j < ncols && is_digit matrix.(i).(!j) do 
    n := !n * 10 + int_of_char matrix.(i).(!j); j := !j + 1
  done; !n

let sum_part_numbers matrix =
  let nrows = Array.length matrix in 
  let ncols = Array.length (Array.get matrix 0) in 
  let sum = ref 0 in 
  for i = 0 to nrows - 1 do 
    for j = 0 to ncols - 1 do 
      let n = if is_part_number matrix i j then get_number matrix i j else 0 in 
      sum := !sum + n
    done
  done;
  !sum

let gear_ratio matrix i j =
  let leftmost_digi matrix i j =
    let j = ref j in 
    while !j > 0 && is_digit matrix.(i).(!j - 1) do 
      j := !j - 1
    done; !j
  in
  let get_number matrix i j = 
    let j = leftmost_digi matrix i j in 
    get_number matrix i j
  in
  let is_digit matrix i j = 
    i >= 0 && i < Array.length matrix && j >= 0 && j < Array.length matrix.(0) && is_digit matrix.(i).(j)
  in
  if not @@ is_symbol matrix.(i).(j) then 0 else
    let ns = [] in 
    let ns = if is_digit matrix (i - 1) (j - 1) then get_number matrix (i - 1) (j - 1) :: ns else ns in
    let ns = if is_digit matrix (i) (j - 1) then get_number matrix (i) (j - 1) :: ns else ns in
    let ns = if is_digit matrix (i + 1) (j - 1) then get_number matrix (i + 1) (j - 1) :: ns else ns in
    let ns = if is_digit matrix (i) (j + 1) then get_number matrix (i) (j + 1) :: ns else ns in
    let ns = if is_digit matrix (i - 1) (j) && not @@ is_digit matrix (i - 1) (j - 1) then get_number matrix (i - 1) (j) :: ns else ns in
    let ns = if is_digit matrix (i + 1) (j) && not @@ is_digit matrix (i + 1) (j - 1) then get_number matrix (i + 1) (j) :: ns else ns in
    let ns = if is_digit matrix (i - 1) (j + 1) && not @@ is_digit matrix (i - 1) (j) then get_number matrix (i - 1) (j + 1) :: ns else ns in
    let ns = if is_digit matrix (i + 1) (j + 1) && not @@ is_digit matrix (i + 1) (j) then get_number matrix (i + 1) (j + 1) :: ns else ns in
    if List.length ns = 2 then List.hd ns * List.nth ns 1 else 0

let sum_gear_ratios matrix =
  let sum = ref 0 in 
  Array.iteri (fun i row -> Array.iteri (fun j _ -> let ratio = gear_ratio matrix i j in sum := !sum + ratio) row) matrix;
  !sum

let () =
  let lines = read_lines () in
  let matrix = build_matrix lines in
  let ans1 = sum_part_numbers matrix in 
  let ans2 = sum_gear_ratios matrix in
  Printf.printf "Answer to part 1 is %d\nAnswer to part 2 is %d\n" ans1 ans2
