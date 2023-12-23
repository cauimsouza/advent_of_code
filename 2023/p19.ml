type dimension = X | M | A | S

type state = string

type rule = 
  | Less of dimension * int * state (* ("a", 2006, "qkq") *)
  | Greater of dimension * int * state

type workflow = {
  name: state;
  rules: rule list;
  catchall: state
}

let string_of_dimension = function 
  | X -> "x"
  | M -> "m"
  | A -> "a"
  | S -> "s"

let string_of_rule = function 
  | Less (dim, threshold, state) ->
      Printf.sprintf "%s<%d:%s" (string_of_dimension dim) threshold state 
  | Greater (dim, threshold, state) -> 
      Printf.sprintf "%s>%d:%s" (string_of_dimension dim) threshold state 

let string_of_workflow { name; rules; catchall } = 
  let name = Printf.sprintf "name: %s" name in 
  let rules = Printf.sprintf "rules: %s" (String.concat "," (List.map string_of_rule rules)) in
  let catchall = Printf.sprintf "catchall: %s" catchall in
  String.concat "\n" [name; rules; catchall]

let string_of_ratings ratings =
  let kvs = Hashtbl.fold (fun k v acc -> (Printf.sprintf "%s:%d" (string_of_dimension k) v) :: acc) ratings [] in
  "{" ^ (String.concat "," kvs) ^ "}"

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

let workflow_of_str str =
  let name_len = String.index_from str 0 '{' in 
  let name = String.sub str 0 name_len in 

  let rules = String.sub str (name_len + 1) (String.length str - (name_len + 2)) in
  let rules = String.split_on_char ',' rules in 
  let rules = List.rev rules in 
  let catchall, rules = (List.hd rules, List.tl rules |> List.rev) in 

  let parse_rule str =
    let dim = match str.[0] with 
      | 'x' -> X 
      | 'm' -> M
      | 'a' -> A 
      | 's' -> S
      | _ -> assert false
    in

    let len = String.length str in

    let comma_id = String.index_from str 0 ':' in
    let dst = String.sub str (comma_id + 1) (len - comma_id - 1) in

    let threshold = int_of_string (String.sub str 2 (comma_id - 2)) in

    match str.[1] with 
    | '>' -> Greater (dim, threshold, dst)
    | '<' -> Less (dim, threshold, dst)
    | _ -> assert false
  in

  let rules = List.map parse_rule rules in
  { name = name; rules = rules; catchall = catchall }

let ratings_of_str str =
  let ratings = Hashtbl.create 4 in

  let str = String.sub str 1 (String.length str - 2) in 
  let dim_vals = String.split_on_char ',' str in

  let parse_dim_val dim_val =
    let dim =
      match dim_val.[0] with 
      | 'x' -> X 
      | 'm' -> M 
      | 'a' -> A 
      | 's' -> S
      | _ -> assert false 
    in
    let value = int_of_string (String.sub dim_val 2 (String.length dim_val - 2)) in
    Hashtbl.add ratings dim value
  in

  List.iter parse_dim_val dim_vals;
  ratings

let parse_lines lines =
  let workflows_of_list (workflows_list : workflow list) =
    let workflows = Hashtbl.create 100 in 
    List.iter (fun w -> Hashtbl.add workflows w.name w) workflows_list;
    workflows
  in

  let rec split_lines workflows lines =
    match lines with
    | l :: ls when l = "" ->
      (workflows, ls)
    | l :: ls ->
      split_lines (l :: workflows) ls
    | _ -> assert false
  in

  let (workflows_strs, ratings_strs) = split_lines [] lines in
  let workflows = workflows_of_list (List.map workflow_of_str workflows_strs) in
  let ratings = List.map ratings_of_str ratings_strs in
  (workflows, ratings)

let match_rule rule ratings =
  match rule with 
  | Less (dim, threshold, state) ->
    let value = Hashtbl.find ratings dim in 
    if value < threshold then Some state
    else None

  | Greater (dim, threshold, state) ->
    let value = Hashtbl.find ratings dim in 
    if value > threshold then Some state
    else None

let next_workflow workflow ratings =
  let rec match_rules rem_rules =
    match rem_rules with 
    | [] -> workflow.catchall
    | x :: xs ->
      match match_rule x ratings with
      | None -> match_rules xs 
      | Some s -> s 
  in 
  match_rules workflow.rules

let accepts workflows ratings = 
  let module SSet = Set.Make(String) in
  let rec visit_workflow workflow visited =
    let visited = SSet.add workflow.name visited in 

    let next_workflow = next_workflow workflow ratings in
    if next_workflow = "A" then true 
    else if next_workflow = "R" then false
    else if SSet.mem next_workflow visited then false
    else
      let workflow = Hashtbl.find workflows next_workflow in 
      visit_workflow workflow visited
  in

  let starting_workflow = Hashtbl.find workflows "in" in 
  visit_workflow starting_workflow SSet.empty

let ratings_score workflows ratings_list = 
  let score ratings = 
    Hashtbl.fold (fun k v acc -> acc + v) ratings 0
  in
  let accepting_ratings = List.filter (accepts workflows) ratings_list in
  let scores = List.map score accepting_ratings in
  List.fold_left (+) 0 scores

let () =
  let lines = read_lines "input.txt" in 
  let (workflows, ratings) = parse_lines lines in 
  let ans1 = ratings_score workflows ratings in
  Printf.printf "Answer to part 1 is %d\n" ans1
