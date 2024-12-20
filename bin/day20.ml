open Core;;

module Direction = struct
  type t = North | East | South | West
  [@@deriving eq,compare,hash,sexp];;
  let all = [North; East; South; West];;
  let idx = function | North -> 0 | East -> 1 | South -> 2 | West -> 3
  let _ = idx;;
  let of_idx = function | 0 -> North | 1 -> East | 2 -> South | 3 -> West | _ -> failwith "Invalid direction"
  let _ = of_idx;;

  let clockwise = function | North -> East | East -> South | South -> West | West -> North
  let _ = clockwise;;
  let counterclockwise = function | North -> West | West -> South | South -> East | East -> North
  let _ = counterclockwise;;
  
  let diff = function
    | North -> (function | North -> 0 | (East | West) -> 1 | South -> 2)
    | East  -> (function | (North | South) -> 1 | East -> 0 | West -> 2)
    | South -> (function | North -> 2 | (East | West) -> 1 | South -> 0)
    | West  -> (function | (North | South) -> 1 | East -> 1 | West -> 0);;
  let _ = diff;;
end;;

module Pos = struct
  type t = int * int [@@deriving eq,compare,hash,sexp];;
  let dist (r1,c1) (r2,c2) = (abs (r2 - r1)) + (abs (c2 - c1));;
  let to_string (x,y) = sprintf "(%d,%d)" x y;;
end;;

open Direction;;

let step dir (r,c) =
  match dir with
  | North -> (r-1,c)
  | South -> (r+1,c)
  | East -> (r, c+1)
  | West -> (r, c-1)
;;
let read_input f =
    In_channel.read_lines f
    |> List.foldi ~init:((None, None), [])
           ~f:(fun ridx ((epos, spos), rows) s ->
                  let arr = Array.create ~len:(String.length s) false in
                  (String.foldi ~init:(epos,spos) s
                       ~f:(fun cidx (epos, spos) -> function
                              | '.' -> (epos,spos)
                              | '#' -> arr.(cidx) <- true; (epos,spos)
                              | 'E' -> (Some (ridx,cidx), spos)
                              | 'S' -> (epos, Some (ridx,cidx))
                              | _ -> failwith "Invalid map char"),
                   arr::rows))
    |> function
    | ((Some epos, Some spos), rows) -> (List.rev rows |> Array.of_list,epos,spos)
    | _ -> failwith "Invalid map"
;;

let inbounds map (x,y) = (x >= 0) && (x < Array.length map) && (y >= 0) && (y < Array.length map.(0));;
let is_wall map (x,y) =
    if inbounds map (x,y)
    then map.(x).(y)
    else true
;;

let find_path map epos spos =
    let rec helper acc pos =
        if Pos.equal epos pos
        then List.rev (epos::acc)
        else List.find_map_exn Direction.all
                 ~f:(fun dir -> let p = step dir pos in
                        if not (is_wall map p) && not (List.hd acc
                                                       |> Option.map ~f:(fun prev -> Pos.equal prev p)
                                                       |> Option.value ~default:false)
                        then Some p else None)
             |> helper (pos::acc) in
    helper [] spos

let count map steps epos pos =
    let cache = Hashtbl.create (module Pos) in
    let rec helper steps = function
        | [] -> ()
        | frontier ->
            List.fold_left frontier ~init:[]
                ~f:(fun frontier pos ->
                       Hashtbl.set cache ~key:pos ~data:steps;
                       List.fold_left Direction.all ~init:frontier
                           ~f:(fun frontier dir ->
                                  let p = step dir pos in
                                  if not (is_wall map p) && Option.is_none (Hashtbl.find cache p)
                                  then p::frontier else frontier))
            |> helper (steps+1)
    in
    helper steps [pos];
    Hashtbl.find_exn cache epos
;;

let cheats_at_loc cheat_len map pos =
    let rec helper acc h v =
        if h > cheat_len
        then acc
        else if (abs h) + (abs v) > cheat_len
        then helper acc (h+1) (-1 * (cheat_len - (abs (h+1))))
        else
            let pos = (fst pos + h, snd pos + v) in
            if not (is_wall map pos)
            then helper (pos::acc) h (v+1)
            else helper acc h (v+1)
    in
    helper [] (-cheat_len) 0
        
    (* List.fold_left (List.cartesian_product Direction.all Direction.all) ~init:[] *)
    (*     ~f:(fun cheats (d1, d2) -> *)
    (*            let p1 = step d1 pos in *)
    (*            let p2 = step d2 p1 in *)
    (*            if inbounds map p1 && is_wall map p1 && not (is_wall map p2) && not (Pos.equal p2 pos) *)
    (*            then p2::cheats *)
    (*            else cheats) *)
;;

let pmap ?(f = (fun _ _ -> '.')) =
    Array.iteri ~f:(fun ri row ->
                      Array.iteri row ~f:(fun ci -> function
                                            | true -> Out_channel.output_char stdout '#'
                                            | _    -> Out_channel.output_char stdout (f ri ci));
                      Out_channel.newline stdout);;
let _ = pmap;;

let count_good_cheats cheat_len map epos spos =
    let cache = Hashtbl.create (module Pos) in
    let path = find_path map epos spos in
    let orig_len = List.length path in
    let () = printf "Start = %s End = %s Orig Map is (%d,%d)"
                 (Pos.to_string spos) (Pos.to_string epos)
                 (Array.length map) (Array.length map.(0)) in
    let () = printf "Path length is %d\n" orig_len in
    (* let () = pmap map in *)
    path
    |> List.foldi ~init:0
        ~f:(fun steps cnt pos ->
               cheats_at_loc cheat_len map pos
               |> List.fold_left ~init:cnt
                      ~f:(fun cnt pos' ->
                             let cheat = Pos.dist pos pos' in
                             match Hashtbl.find cache pos' with
                             | Some dist ->
                                 if (steps + cheat + dist) <= orig_len - 100 then cnt+1 else cnt
                             | None ->
                                 let dist = count map 0 epos pos' in
                                 Hashtbl.set cache ~key:pos' ~data:dist;
                                 if (steps + cheat + dist) <= orig_len - 100 then cnt+1 else cnt))
;;

let part_a () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> (fun (map,epos,spos) -> count_good_cheats 2 map epos spos)
    |> printf "Found %d good cheats\n";;

part_a ();;

let part_b () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> (fun (map,epos,spos) -> count_good_cheats 20 map epos spos)
    |> printf "Found %d good cheats\n";;

part_b ();;
    
