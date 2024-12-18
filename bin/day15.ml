open Core;;
type cell = Wall | Box | Empty;;
type map = cell Array.t Array.t;;
type instr = North | South | West | East [@@deriving eq];;

let apply (row,col) = function
  | North -> (row-1,col)
  | South -> (row+1,col)
  | West  -> (row,col-1)
  | East  -> (row,col+1)

type readstate =
  | ReadingMap of ((int * int) option * cell Array.t List.t)
  | ReadingInstructions of ((int * int) * map * instr List.t)
;;
                           
let read_input f =
  let read_map_line l =
    String.to_list l
    |> List.foldi ~init:(None, []) ~f:(
      fun idx (pos,xs) -> function
        | '#' -> (pos, Wall::xs)
        | 'O' -> (pos, Box::xs)
        | '@' -> (Some idx, Empty::xs)
        | '.' -> (pos, Empty::xs)
        | c   -> failwith (sprintf "Invalid map character '%c'" c))
    |> fun (pos, xs) -> (pos, List.rev xs |> Array.of_list) in
  In_channel.read_lines f
  |> List.foldi
    ~init:(ReadingMap (None, []))
    ~f:(fun ridx ->
        function
        | ReadingMap (pos, rows) -> fun l ->
          if String.(strip l |> length) = 0
          then (match pos with
              | Some pos -> ReadingInstructions (pos, Array.of_list (List.rev rows), [])
              | None -> failwith "No robot detected")
          else let (off,row) = read_map_line l in
            (match pos,off with
             | None, Some cidx -> ReadingMap (Some (ridx,cidx), row::rows)
             | Some _, Some _  -> failwith "Duplicate robot detected"
             | _ -> ReadingMap (pos, row::rows))
        | ReadingInstructions (pos, map, instrs) ->
          fun l -> 
            ReadingInstructions
              (pos, map,
               String.fold l ~init:instrs ~f:(fun instrs -> function
                   | '<' -> West::instrs
                   | '>' -> East::instrs
                   | '^' -> North::instrs
                   | 'v' -> South::instrs
                   | c -> failwith (sprintf "Unknown instruction '%c'\n" c)))
          )
  |> function
  | ReadingMap _ -> failwith "Couldn't find end of map"
  | ReadingInstructions (pos,map,instrs) -> (pos,map, List.rev instrs)
;;

let point_equal (r0,c0) (r1,c1) = r0 = r1 && c0 = c1;;

let rec find_free_location map instr pos =
  let pos' = apply pos instr in
  try
    match map.(fst pos').(snd pos') with
    | Empty -> Some pos'
    | Box   -> find_free_location map instr pos'
    | Wall  -> None
  with _ -> None
    
let rec run map pos = function
  | [] -> map
  | instr::rest ->
    match find_free_location map instr pos with
    | Some (row',col') -> let new_pos = apply pos instr in
      if point_equal new_pos (row', col')
      then ()
      else
        (map.(fst new_pos).(snd new_pos) <- Empty;
         map.(row').(col') <- Box);
      run map new_pos rest
    | None -> run map pos rest

let map_foldi ~init ~f map =
  Array.foldi ~init map
    ~f:(fun ridx acc -> Array.foldi ~init:acc ~f:(fun cidx acc -> f (ridx,cidx) acc))
;;

let compute_gps_sum =
  map_foldi ~init:0 
    ~f:(fun (ridx,cidx) sum -> function
        | Box -> sum + (100 * ridx + cidx)
        | _   -> sum)
    
let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (pos,map,instrs) -> run map pos instrs)
  |> compute_gps_sum
  |> printf "GPS sum is %d\n"
;;

part_a ();;

type wide_cell = W_Wall | W_BoxLeft | W_BoxRight | W_Empty;;
let widen m pos =
  Array.init (Array.length m)
    ~f:(fun ridx ->
        Array.init (2*(Array.length m.(0)))
          ~f:(fun cidx ->
              match m.(ridx).(cidx/2) with
              | Empty -> W_Empty
              | Wall  -> W_Wall
              | Box   -> if cidx %2 = 0 then W_BoxLeft else W_BoxRight)),
  (fst pos, 2*(snd pos))
;;

let print_wide_map pos map =
  Array.iteri ~f:(fun ridx r -> Array.iteri r ~f:(fun cidx -> function
      | W_Wall -> Out_channel.output_char stdout '#'
      | W_Empty -> Out_channel.output_char stdout (if point_equal (ridx,cidx) pos then '@' else '.')
      | W_BoxLeft -> Out_channel.output_char stdout '['
      | W_BoxRight -> Out_channel.output_char stdout ']');
      Out_channel.newline stdout) map;
  Out_channel.flush stdout
    
let get map (row,col) = map.(row).(col)
let set map (row,col) v = map.(row).(col) <- v;;
             
let compare_pos (a,b) (c,d) = let x = Int.compare a c in
  if x = 0 then Int.compare b d else x;;
let _ = print_wide_map;;

let rec wide_run map pos =
  function
  | [] -> map
  | instr::instrs ->
    (* let () = print_wide_map pos map in *)
    let orig_pos = pos in
    let rec move_ns (k : unit -> (int * int)) frontier =
      let frontier' = List.map ~f:(fun pos -> apply pos instr) frontier in
      if List.exists frontier' ~f:(fun pos -> get map pos |> function |W_Wall -> true | _ -> false)
      then orig_pos
      else
        let frontier' = List.concat_map frontier' ~f:(fun pos ->
            match get map pos with 
            | W_Empty -> []
            | W_BoxLeft -> [pos; apply pos East]
            | W_BoxRight -> [pos; apply pos West]
            | W_Wall -> [])
                        |> List.dedup_and_sort ~compare:compare_pos in
        let k = (fun () -> List.iter ~f:(fun pos ->
            let new_pos = apply pos instr in
            set map new_pos (get map pos);
            set map pos W_Empty;
            ) frontier; k ()) in
        match frontier' with
        | [] -> k ()
        | _  -> move_ns k frontier' in
    let rec move_east k pos =
      match get map pos with
      | W_Empty   -> k ()
      | W_Wall    -> orig_pos
      | W_BoxLeft -> move_east (fun () ->
          let br      = apply pos instr in
          let new_pos = apply br instr in
          set map br W_BoxLeft;
          set map new_pos W_BoxRight;
          set map pos W_Empty;
          k ()) (apply (apply pos instr) instr)
      | W_BoxRight -> failwith "Traveling East hit BoxRight" in
    let rec move_west k pos =
      match get map pos with
      | W_Empty   -> k ()
      | W_Wall    -> orig_pos
      | W_BoxLeft -> failwith "Traveling West hit BoxLeft"
      | W_BoxRight -> move_west (fun () ->
          let bl      = apply pos instr in
          let new_pos = apply bl instr in
          set map bl W_BoxRight;
          set map new_pos W_BoxLeft;
          set map pos W_Empty;
          k ()) (apply (apply pos instr) instr)
    in
    match instr with
    | (North | South) -> wide_run map (move_ns (fun () -> apply orig_pos instr) [pos]) instrs
    | East   -> wide_run map (move_east (fun () -> apply orig_pos instr) (apply pos instr)) instrs
    | West   -> wide_run map (move_west (fun () -> apply orig_pos instr) (apply pos instr)) instrs

let compute_wide_gps_sum =
  map_foldi ~init:0 
    ~f:(fun (ridx,cidx) sum -> function
        | W_BoxLeft -> sum + (100 * ridx + cidx)
        | _   -> sum)
    
let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (pos,map,instrs) ->
      let (map,pos) = widen map pos in
      (* let () = print_wide_map pos map in *)
      wide_run map pos instrs)
  |> compute_wide_gps_sum
  |> printf "GPS sum is %d\n"
;;

part_b ();;

;;

          
                
