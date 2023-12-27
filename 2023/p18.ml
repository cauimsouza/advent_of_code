type coord = int * int

type dir = L | R | U | D

type move = dir * int

let dir_of_string = function 
  | "L" -> L
  | "R" -> R
  | "U" -> U
  | "D" -> D
  | _ -> assert false

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

let move_of_line_p1 line =
  let subs = String.split_on_char ' ' line in
  let (d, s, _) = match subs with 
    | d :: s :: h :: [] -> (dir_of_string d, int_of_string s, h)
    | _ -> assert false
  in (d, s)

let move_of_line_p2 line =
  let subs = String.split_on_char ' ' line in
  let (_, _, h) = match subs with 
    | d :: s :: h :: [] -> (dir_of_string d, int_of_string s, h)
    | _ -> assert false
  in
  let dir = match h.[7] with 
    | '0' -> R | '1' -> D | '2' -> L | '3' -> U | _ -> assert false
  in
  let s = String.sub h 2 5 in
  let s = String.fold_left (fun acc c ->
    let d = match c with
    | '0'..'9' -> int_of_char c - int_of_char '0' 
    | _ -> int_of_char c - int_of_char 'a' + 10
    in
    acc * 16 + d) 0 s in
  (dir, s)

let points_of_lines move_of_line lines =
  let moves = List.map move_of_line lines in

  let rec points_of_moves moves (x, y) points =
    match moves with
    | [] -> points
    | (L, s) :: moves ->
      let p = (x - s, y) in
      points_of_moves moves p (p :: points)
    | (R, s) :: moves ->
      let p = (x + s, y) in
      points_of_moves moves p (p :: points)
    | (U, s) :: moves ->
      let p = (x, y - s) in 
      points_of_moves moves p (p :: points)
    | (D, s) :: moves ->
      let p = (x, y + s) in 
      points_of_moves moves p (p :: points)
  in
  points_of_moves moves (0, 0) [(0, 0)]
  |> List.rev

let area points =
  let open Int64 in
  let points = List.map (fun (x, y) -> (of_int x, of_int y)) points in
  let det (x, y) (x', y') =
    sub (mul x y') (mul x' y)
  in
  let rec shoelace points hd acc =
    match points with
    | p :: [] -> div (add acc (det p hd)) 2L
    | p1 :: (p2 :: ps as tl) ->
      shoelace tl hd (add acc (det p1 p2))
    | _ -> assert false
  in
  let rec perimeter points hd acc =
    match points with
    | (x, y) :: [] -> add acc (add (abs (sub x (fst hd))) (abs (sub y (snd hd))))
    | (x, y) :: ((x', y') :: ps as tl) ->
      perimeter tl hd (add acc (add (abs (sub x x')) (abs (sub y y'))))
    | _ -> assert false
  in

  let hd = List.hd points in 
  let a = shoelace points hd 0L in
  let p = perimeter points hd 0L in
  add a (add (div (sub p 4L) 2L) 3L)

let () =
  let lines = read_lines "input.txt" in
  let points1 = points_of_lines move_of_line_p1 lines in 
  Printf.printf "Answer to part 1 is %s\n" (area points1 |> Int64.to_string);
  let points2 = points_of_lines move_of_line_p2 lines in 
  Printf.printf "Answer to part 2 is %s\n" (area points2 |> Int64.to_string);
