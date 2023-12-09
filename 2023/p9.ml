let read_file filename = 
  let ch = open_in filename in 
  let read_line () = 
    try Some (input_line ch) with End_of_file -> None 
  in 
  let rec read_lines lines = 
    match read_line () with 
    | None -> lines 
    | Some line -> read_lines (line :: lines)
  in 
  read_lines []

let line_to_seq line = 
  let seqs = String.split_on_char ' ' line in 
  List.map int_of_string seqs

let rec extrapolate seq =
  let rec diffs = function
    | [x; y] -> [y - x]
    | x :: (y :: ys as tl) -> (y - x) :: diffs tl
    | _ -> assert false
  in
  let last xs = List.nth xs (List.length xs - 1) in
  if List.for_all (fun n -> n = 0) seq then 0 else last seq + extrapolate (diffs seq)

let () =
  let lines = read_file "input_p9.txt" in 
  let seqs = List.map line_to_seq lines in 
  let ans1 = List.fold_left (fun acc s -> acc + extrapolate s) 0 seqs in 
  let ans2 = List.fold_left (fun acc s -> acc + extrapolate (List.rev s)) 0 seqs in 
  Printf.printf "Answer to part 1 is %d\nAnswer to part 2 is %d\n" ans1 ans2
