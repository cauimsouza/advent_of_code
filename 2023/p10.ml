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
  List.rev (read_lines [])

let build_grid lines = 
  let nrows = List.length lines in
  Array.init nrows (fun i -> List.nth lines i |> String.to_seq |> Array.of_seq)
    
let find_start grid =
  let rec scan_row i =
    match Array.find_index ((=) 'S') grid.(i) with 
    | Some j -> (i, j)
    | None -> scan_row (i + 1)
  in 
  scan_row 0

let adjacent grid (i, j) (i', j') =
  let c = grid.(i).(j) in 
  let c' = grid.(i').(j') in 
  if i' = i - 1 then 
    (c = 'S' || c = '|' || c = 'L' || c = 'J') && (c' = 'S' || c' = '|' || c' = '7' || c' = 'F')
  else if i' = i + 1 then 
    (c' = 'S' || c' = '|' || c' = 'L' || c' = 'J') && (c = 'S' || c = '|' || c = '7' || c = 'F')
  else if j' = j - 1 then 
    (c = 'S' || c = '-' || c = '7' || c = 'J') && (c' = 'S' || c' = '-' || c' = 'L' || c' = 'F')
  else (*j' = j + 1*)
    (c' = 'S' || c' = '-' || c' = '7' || c' = 'J') && (c = 'S' || c = '-' || c = 'L' || c = 'F')

let build_dists grid =
  let nrows = Array.length grid in 
  let ncols = Array.length grid.(0) in 

  let dists = Array.make_matrix nrows ncols (-1) in
  let (i0, j0) = find_start grid in 
  let to_explore = Queue.create() in 
  dists.(i0).(j0) <- 0;
  Queue.push (i0, j0) to_explore;

  let rec loop () =
    if Queue.is_empty to_explore then ()
    else
      let node = Queue.pop to_explore in 
      explore_neighbours node
  
  and explore_neighbours (i, j) =
    let deltas = [(-1, 0); (0, -1); (0, 1); (1, 0)] in
    let neighbours = List.map (fun (i', j') -> (i' + i, j' + j)) deltas in 
    List.iter (explore_neighbour (i, j)) neighbours;
    loop()
  
  and explore_neighbour ((i, j) as src) ((i', j') as dst) =
    if i' < 0 || i' >= nrows || j' < 0 || j' >= ncols || dists.(i').(j') >= 0 || not (adjacent grid src dst) then ()
    else (
      dists.(i').(j') <- dists.(i).(j) + 1;
      Queue.push dst to_explore
    )
  
  in
  loop();
  dists

let max_distance grid dists =
  Array.fold_left (fun acc arr -> max acc (Array.fold_left max 0 arr)) 0 dists

let num_enclosed grid dists =
  let nrows = Array.length grid in 
  let ncols = Array.length grid.(0) in

  let enclosed = ref 0 in
  for i = 0 to nrows - 1 do
    let inside = ref false in
    let prev = ref None in
    for j = 0  to ncols - 1 do 
      if dists.(i).(j) < 0 && !inside then enclosed := !enclosed + 1
      else if dists.(i).(j) >= 0 then
        if grid.(i).(j) == '|' then inside := not !inside
        else if grid.(i).(j) = 'J' then
          match !prev with
          | Some 'F' -> inside := not !inside
          | _ -> ()
        else if grid.(i).(j) = '7' then
          match !prev with 
          | Some 'L' -> inside := not !inside 
          | _ -> ()
        else if grid.(i).(j) != '-' then prev := Some grid.(i).(j)
        else ()
      else ()
    done
  done;
  !enclosed

let () =
  let input_lines = read_file "input_p10.txt" in
  let grid = build_grid input_lines in
  let dists = build_dists grid in
  max_distance grid dists |> Printf.printf "Answer to part 1 is %d\n";
  num_enclosed grid dists |> Printf.printf "Answer to part 2 is %d\n"
