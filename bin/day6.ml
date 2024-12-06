open Core;;

type map_cell = Obstacle | Clear;;
type direction = North | West | East | South;;

let pos_row (r,_,_) = r;;
let pos_col (_,c,_) = c;;

let idx_of_dir = function
  | North -> 0
  | South -> 1
  | East  -> 2
  | West  -> 3
;;

let map_rows map = Array.length map ;;
let map_cols map = Array.length map.(0);;

let char_for_dir = function
  | North -> '^'
  | South -> 'v'
  | East -> '>'
  | West -> '<'

let print_state map pos =
  let char_for_loc ri ci = match pos with
    | (row, col, dir) -> (if ri = row && ci = col
                          then char_for_dir dir
                          else '.') in
  Array.iteri map ~f:(fun ri row ->
      let open Out_channel in
      Array.iteri row ~f:(fun ci -> function
          | Obstacle -> output_char stdout '#'
          | Clear -> char_for_loc ri ci |> output_char stdout
        ) ; newline stdout)
;;
let _ = print_state;;

let print_path map visited added (r0,c0,d0) =
  let visit_sigil v = (if (v.(0) || v.(1)) && (v.(2) || v.(3))
                       then '+'
                       else if v.(0) || v.(1)
                       then '|'
                       else '-') in
  Array.iteri map ~f:(fun ri row ->
      let open Out_channel in
      Array.iteri row ~f:(fun ci x ->
          output_char stdout
            (if (ri = r0 && ci = c0)
             then char_for_dir d0
             else if Array.exists visited.(ri).(ci) ~f:(fun x -> x)
             then visit_sigil visited.(ri).(ci)
             else match x with
               | Obstacle -> (if (ri = pos_row added  &&
                                  ci = pos_col added)
                              then 'O' else '#')
               | Clear -> '.'));
      newline stdout)
let _ = print_path;;

let read_input f =
  let lines = In_channel.read_lines f in
  let map   = Array.init (List.length lines) ~f:(fun _ -> List.hd_exn lines
                                                          |> String.length
                                                          |> fun len -> Array.create ~len Clear) in
  List.foldi ~init:None ~f:(fun ri pos ->
      String.foldi ~init:pos ~f:(fun ci pos -> function
          | '#' -> (map.(ri).(ci) <- Obstacle;
                    pos)
          | '^' -> Some (ri, ci, North)
          | '<' -> Some (ri, ci, West)
          | '>' -> Some (ri, ci, East)
          | 'v' -> Some (ri, ci, South)
          | _   -> pos)) lines
  |> function
  | Some pos -> (map, pos)
  | _ -> failwith "Guard not found"
;;
        
let get map (row,col,_) =
  try map.(row).(col) |> Option.return
  with _ -> None
;;

let set map (row,col,_) v =
  try map.(row).(col) <- v
  with _ -> ()

let next = function
  | (row,col,North) -> (row-1,col,North)
  | (row,col,South) -> (row+1,col,South)
  | (row,col,East)  -> (row,col+1,East)
  | (row,col,West)  -> (row,col-1,West)
;;

let turn_right = function
  | (row,col,North) -> (row,col,East)
  | (row,col,East)  -> (row,col,South)
  | (row,col,South) -> (row,col,West)
  | (row,col,West)  -> (row,col,North)
                       
let count_locs (map,pos) =
  let visited = Array.init (map_rows map) ~f:(fun _ -> Array.create ~len:(map_cols map) false) in
  let rec helper pos =
    let new_pos = next pos in
    match get map new_pos with
    | None          -> ()
    | Some Obstacle -> helper (turn_right pos)
    | Some Clear    -> (set visited new_pos true;
                        helper new_pos)
  in
  set visited pos true;
  helper pos;
  Array.fold visited ~init:0 ~f:(fun cnt row ->
      Array.fold row ~init:cnt ~f:(fun x -> function
          | true -> x + 1
          | false -> x))
;;

                             
let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> count_locs
  |> printf "The guard visited %d locations.\n"
;;

let map_with_obstacle_at map (row,col,_) =
  Array.init (map_rows map)
    ~f:(fun ri -> Array.init (map_cols map)
           ~f:(fun ci -> (if ri = row && ci = col
                          then Obstacle
                          else map.(ri).(ci))))

let check_cycle ~insert map pos =
  let map = map_with_obstacle_at map insert in
  let never_visited () = Array.init ~f:(fun _ -> false) 4 in
  let visited = Array.init (map_rows map) ~f:(fun _ -> Array.init (map_cols map) ~f:(fun _ -> never_visited ())) in
  let set_visited (ri,ci,dir) = visited.(ri).(ci).(idx_of_dir dir) <- true in
  let has_visited (ri,ci,dir) = visited.(ri).(ci).(idx_of_dir dir) in
  let rec helper pos =
    let new_pos = next pos in
    match get map new_pos with
    | None          -> false
    | Some Obstacle -> helper (turn_right pos)
    | Some Clear    -> (if has_visited pos
                        then (true)
                        else (set_visited pos;
                              helper new_pos))
  in
  helper pos;;

(* Not exactly elegant, but this works for part B, at each step of the
   path check to see if inserting an obstacle to block the next step
   would create a cycle.

   Note that when checking if we create a cycle we need to rerun the
   walk from the beginning since an earlier step may have moved
   through the location in question in a different direction.
*)
let find_cycle_opportunities (map,pos) =  
  let orig_pos = pos in
  let rec helper obs pos =
    let new_pos = next pos in
    match get map new_pos with
    | None          -> obs
    | Some Obstacle -> helper obs (turn_right pos)
    | Some Clear    -> (if check_cycle map ~insert:new_pos orig_pos
                        then helper (new_pos::obs) new_pos
                        else helper obs new_pos)
  in
  helper [] pos
  |> List.dedup_and_sort ~compare:(fun (r1,c1,_) (r2,c2,_) ->
      let rc = Int.compare r1 r2 in
      if rc = 0 then Int.compare c1 c2 else rc)
  |> List.length
;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> find_cycle_opportunities
  |> printf "Possible cycles: %d\n"
;;

part_a ();;
part_b ();;
