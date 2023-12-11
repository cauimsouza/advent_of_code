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

  let grid_of_lines lines =
    let nrows = List.length lines in 
    Array.init nrows (fun i -> List.nth lines i |> String.to_seq |> Array.of_seq)

let find_galaxies grid = 
  let galaxies = ref [] in
  Array.iteri (fun i row ->
    Array.iteri (fun j c ->
      if c = '#' then galaxies := (i, j) :: !galaxies else ()) row
    ) grid;
  !galaxies

let sum_distances grid multiplier =
  let nrows = Array.length grid in 
  let ncols = Array.length grid.(0) in 

  let galaxies = find_galaxies grid in 

  let empty_rows = Array.make nrows 1 in 
  let empty_cols = Array.make ncols 1 in 
  List.iter (fun (i, j) ->
    empty_rows.(i) <- 0;
    empty_cols.(j) <- 0) galaxies;
  for i = 1 to nrows - 1 do
    empty_rows.(i) <- empty_rows.(i) + empty_rows.(i - 1)
  done;
  for j = 1 to ncols - 1 do 
    empty_cols.(j) <- empty_cols.(j) + empty_cols.(j - 1)
  done;

  let dist (i, j) (i', j') =
    let erows = (multiplier - 1) * abs (empty_rows.(i') - empty_rows.(i)) in 
    let ecols = (multiplier - 1) * abs (empty_cols.(j') - empty_cols.(j)) in 
    abs (i - i') + abs (j - j') + erows + ecols
  in

  let rec sum_distances = function 
  | [] -> 0
  | g :: gs ->
    let s = List.fold_left (fun acc gal -> acc + dist gal g) 0 gs in
    s + sum_distances gs
  in
  sum_distances galaxies

let () =
  let lines = read_file "input.txt" in 
  let grid = grid_of_lines lines in 
  sum_distances grid 2 |> Printf.printf "Answer to part 1 is %d\n";
  sum_distances grid 1000000 |> Printf.printf "Answer to part 2 is %d\n"