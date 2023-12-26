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

let string_of_interval intervals =
  let dims = [X; M; A; S] in
  let ss = List.map (fun dim ->
    let (a, b) = Hashtbl.find intervals dim in 
    Printf.sprintf "%s: (%d, %d)" (string_of_dimension dim) a b) dims in 
  "{" ^ (String.concat "," ss) ^ "}"

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
    Hashtbl.fold (fun _ v acc -> acc + v) ratings 0
  in
  let accepting_ratings = List.filter (accepts workflows) ratings_list in
  let scores = List.map score accepting_ratings in
  List.fold_left (+) 0 scores

let print_autoreferecing_workflows workflows =
  let autorefs { name; rules; catchall } = 
    let same_name = function 
    | Less (_, _, nm) -> nm = name 
    | Greater (_, _, nm) -> nm = name
    in
    List.fold_left (fun acc rule -> acc || same_name rule) false rules
  in
  Printf.printf "Hello, world!\n";
  Hashtbl.iter (fun name worfklow ->
    if autorefs worfklow then Printf.printf "%s\n" name else ()) workflows

type dfs_state = Unvisited | Visiting | Visited

(*This proved there are no cycles in the graph*)
let find_cycles workflows =
  let rule_neighbour rule =
    match rule with 
    | Less (_, _, neighbour) -> neighbour 
    | Greater (_, _, neighbour) -> neighbour
  in
  let rec visit states workflows workflow_name =
    let state = Hashtbl.find states workflow_name in
    match state with 
    | Visited -> ()
    | Visiting -> Printf.printf "Loop found for %s\n" workflow_name
    | Unvisited -> (
      Hashtbl.add states workflow_name Visiting;
      let { rules } = Hashtbl.find workflows workflow_name in
      let neighbours = List.map rule_neighbour rules in
      List.iter (visit states workflows) neighbours;
      Hashtbl.add states workflow_name Visited
    )
  in

  let states = Hashtbl.create 10 in
  Hashtbl.iter (fun k _ -> Hashtbl.add states k Unvisited) workflows;
  Hashtbl.add states "A" Visited;
  Hashtbl.add states "R" Visited;

  Hashtbl.iter (fun k _ -> visit states workflows k) workflows

let rule_dst = function 
  | Less (_, _, dst) -> dst
  | Greater (_, _, dst) -> dst

let full_interval () =
  let dims = [X; M; A; S] in
  let i = Hashtbl.create 4 in 
  List.iter (fun dim -> Hashtbl.add i dim (1, 4000)) dims;
  i

let inter_intersec_rule i rule = 
  match i with 
  | None -> (None, None)
  | Some i as i0 ->
    match rule with 
    | Less (dim, threshold, _) ->
      let (a, b) = Hashtbl.find i dim in 
      let b' = min b (threshold - 1) in 
      if b' < a then (None, i0)
      else if b' >= b then (i0, None)
      else ( (* a <= b' < b *)
        let j = Hashtbl.copy i in
        Hashtbl.add j dim (a, b');
        let k = Hashtbl.copy i in
        Hashtbl.add k dim (b' + 1, b);
        (Some j, Some k)
      )
    | Greater (dim, threshold, _) ->
      let (a, b) = Hashtbl.find i dim in 
      let a' = max a (threshold + 1) in 
      if a' > b then (None, i0)
      else if a' <= a then (i0, None)
      else ( (* a < a' <= b *)
        let j = Hashtbl.copy i in
        Hashtbl.add j dim (a', b);
        let k = Hashtbl.copy i in 
        Hashtbl.add k dim (a, a' - 1);
        (Some j, Some k)
      )

let rec inter_intersec_inter (a, b) (a', b') =
  if a > a' then inter_intersec_inter (a', b') (a, b)
  else
    if b < a' then None
    else 
      let b'' = min b b' in 
      Some (a', b'')

let inter_intersec_ratings i ratings  =
  let is_none = function
  | None -> true 
  | _ -> false 
  in
  match i with 
  | None -> None 
  | Some i ->
    let j = Hashtbl.create 4 in 
    let dims = [X; M; A; S] in
    let ints = List.map (fun dim ->
      let a = Hashtbl.find i dim in 
      let b = Hashtbl.find ratings dim in 
      (dim, inter_intersec_inter a b)) dims in
    let empty = List.fold_left (fun acc (_, i) -> acc || is_none i) false ints in 
    if empty then None
    else (
      let ints = List.map (fun (dim, i) -> match i with Some i -> (dim, i) | None -> assert false) ints in
      List.iter (fun (dim, i) -> Hashtbl.add j dim i) ints;
      Some j
    )

let accepting_ratings workflows =
  let rec accepting_ratings memo workflow =
    match Hashtbl.find_opt memo workflow with 
    | Some rs -> rs
    | None -> (
      let { rules; catchall } = Hashtbl.find workflows workflow in
      let i = Some (full_interval()) in 
      let rs = build_accepting_ratings memo i rules catchall [] in 
      Hashtbl.add memo workflow rs;
      rs
    )

  and build_accepting_ratings memo i rules catchall acc =
    match rules with 
    | [] ->
      let ratings = accepting_ratings memo catchall in 
      let ratings = List.map (fun j -> inter_intersec_ratings i j) ratings in
      let ratings = List.filter (function | None -> false | _ -> true) ratings in
      let ratings = List.map (function | Some x -> x | None -> assert false) ratings in
      ratings @ acc
    | rule :: rules ->
      let (i, c) = inter_intersec_rule i rule in 
      let ratings = accepting_ratings memo (rule_dst rule) in 
      let ratings = List.map (fun j -> inter_intersec_ratings i j) ratings in
      let ratings = List.filter (function | None -> false | _ -> true) ratings in
      let ratings = List.map (function | Some x -> x | None -> assert false) ratings in
      build_accepting_ratings memo c rules catchall (ratings @ acc)
  in

  let memo = Hashtbl.create (Hashtbl.length workflows) in
  Hashtbl.add memo "A" [full_interval()];
  Hashtbl.add memo "R" [];

  accepting_ratings memo "in"

let num_accepting_ratings rs =
  let count inter =
    let dims = [X; M; A; S] in 
    let cs = List.map (fun dim ->
      let (a, b) = Hashtbl.find inter dim in 
      b - a + 1
      ) dims in 
    let cs = List.map Int64.of_int cs in
    List.fold_left Int64.mul 1L cs
  in
  let counts = List.map count rs in 
  List.fold_left Int64.add 0L counts

let () =
  let lines = read_lines "input_p19.txt" in 
  let (workflows, ratings) = parse_lines lines in 
  let ans1 = ratings_score workflows ratings in
  Printf.printf "Answer to part 1 is %d\n" ans1;
  accepting_ratings workflows |>
  num_accepting_ratings |>
  Int64.to_string |>
  Printf.printf "Answer to part 2 is %s\n"
