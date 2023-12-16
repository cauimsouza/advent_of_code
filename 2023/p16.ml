let read_lines filename =
  let ch = open_in filename in
  let get_line () =
    try Some (input_line ch) with End_of_file -> None
  in 
  let rec f acc =
    match get_line () with 
    | None -> List.rev acc
    | Some line -> f (line :: acc)
  in 
  f []

let build_grid lines =
  let nrows = List.length lines in 
  Array.init nrows (fun i -> List.nth lines i |> String.to_seq |> Array.of_seq)

type dir_t = Left | Right | Up | Down

type coord_t = { i : int; j : int }

let int_of_dir = function
  | Left -> 0
  | Right -> 1
  | Up -> 2
  | Down -> 3

let advance_dir c d =
  match c, d with
  | '.', _ -> [d]
  | '-', Right -> [Right]
  | '-', Left -> [Left]
  | '-', _ -> [Left; Right]
  | '|', Up -> [Up]
  | '|', Down -> [Down]
  | '|', _ -> [Up; Down]
  | '\\', Left -> [Up]
  | '\\', Right -> [Down]
  | '\\', Up -> [Left]
  | '\\', Down -> [Right]
  | '/', Left -> [Down]
  | '/', Right -> [Up]
  | '/', Up -> [Right]
  | '/', Down -> [Left]
  | _, _ -> assert false

let advance_coord { i ; j } = function 
  | Left -> { i ; j = j - 1 }
  | Right -> { i; j = j + 1 }
  | Up -> { i = i - 1; j }
  | Down -> { i = i + 1; j }

let advance_cell coord dir c =
  let dirs = advance_dir c dir in 
  List.map (fun d -> (advance_coord coord d, d)) dirs

let num_energised grid ((coords, dirs) as start) = 
  let nrows = Array.length grid in 
  let ncols = Array.length grid.(0) in

  let visited = Array.init nrows (fun _ -> Array.make_matrix ncols 4 false) in

  let queue = Queue.create() in 
  Queue.push start queue;
  visited.(coords.i).(coords.j).(int_of_dir dirs) <- true;

  let rec explore () =
    if Queue.is_empty queue then ()
    else (
      let ({ i ; j } as coord, dir) = Queue.pop queue in 
      let c = grid.(i).(j) in
      let coord_dir_lst = advance_cell coord dir c in 
      List.iter visit coord_dir_lst;
      explore()
    )

  and visit ({ i ; j }, dir) =
    if i < 0 || i >= nrows || j < 0 || j >= ncols then ()
    else if visited.(i).(j).(int_of_dir dir) then ()
    else (
      visited.(i).(j).(int_of_dir dir) <- true;
      Queue.push ({ i; j }, dir) queue
    )
  in
  explore();

  let iter_dirs arr = 
    if Array.exists (fun x -> x) arr then 1 else 0
  in
  let iter_row row = 
    Array.fold_left (fun acc dirs -> acc + iter_dirs dirs) 0 row
  in
  Array.fold_left (fun acc row -> acc + iter_row row) 0 visited

let solve_part_2 grid = 
  let nrows = Array.length grid in 
  let ncols = Array.length grid.(0) in 

  let left_border = List.init nrows (fun i -> ({ i; j = 0}, Right)) in 
  let right_border = List.init nrows (fun i -> ({ i; j = ncols - 1}, Left)) in 
  let top_border = List.init ncols (fun j -> ({i = 0; j}, Down)) in 
  let bottom_border = List.init ncols (fun j -> ({i = nrows - 1; j}, Up)) in 
  let starters = left_border @ right_border @ top_border @ bottom_border in 

  List.fold_left (fun acc start -> num_energised grid start |> max acc) 0 starters

let () =
  let lines = read_lines "input_p16.txt" in 
  let grid = build_grid lines in 
  num_energised grid ({ i = 0 ; j = 0 }, Right) |> Printf.printf "Answer to part 1 is %d\n";
  solve_part_2 grid |> Printf.printf "Answer to part 2 is %d\n"
