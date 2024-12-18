open Core;;

module Direction = struct
  type t = North | East | South | West
  [@@deriving eq,compare,hash,sexp];;
  let idx = function | North -> 0 | East -> 1 | South -> 2 | West -> 3
  let of_idx = function | 0 -> North | 1 -> East | 2 -> South | 3 -> West | _ -> failwith "Invalid direction"
  let _ = of_idx;;

  let clockwise = function | North -> East | East -> South | South -> West | West -> North
  let counterclockwise = function | North -> West | West -> South | South -> East | East -> North
  let diff = function
    | North -> (function | North -> 0 | (East | West) -> 1 | South -> 2)
    | East  -> (function | (North | South) -> 1 | East -> 0 | West -> 2)
    | South -> (function | North -> 2 | (East | West) -> 1 | South -> 0)
    | West  -> (function | (North | South) -> 1 | East -> 1 | West -> 0)
  ;;
end;;

module Pos = struct
  type t = int * int [@@deriving eq,compare,hash,sexp];;
end;;

module PosDir = struct
  type t = Pos.t * Direction.t [@@deriving eq,compare,hash,sexp];;
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
    | ((Some epos, Some spos), rows) -> (List.rev rows |> Array.of_list,epos,spos, East)
    | _ -> failwith "Invalid map"
;;

let search (map, epos, spos, dir) =
    let cache = Array.init (Array.length map)
                    ~f:(fun _ -> Array.init (Array.length map.(0))
                                     ~f:(fun _ -> Array.create ~len:4 None)) in
    let cget pos dir   = cache.(fst pos).(snd pos).(idx dir) in
    let cset pos dir v = cache.(fst pos).(snd pos).(idx dir) <- (Some v)in
    let mget pos _dir  = map.(fst pos).(snd pos) in
    let steps score pos dir  =
        if Pos.equal epos pos then []
        else
            let r = (score + 1000, pos, pos, clockwise dir)::(score + 1000, pos, pos, counterclockwise dir)::[] in
            let pos' = step dir pos in
            if mget pos' dir then r else (score+1,pos,pos',dir)::r in
    let rec helper = function
        | [] -> ()
        | (score,prev,pos,dir)::q ->
            match cget pos dir with
              | Some (prevs,s) -> (if s = score
                                   then (cset pos dir (prev::prevs, score);
                                         helper q)
                                   else if s < score
                                   then helper q
                                   else (cset pos dir (prev::[], score);
                                         helper ((steps score pos dir)@q)))
            | None -> (cset pos dir (prev::[], score);
                       helper ((steps score pos dir)@q))
    in
    helper [(0,spos,spos,dir)];
    cache

let option_min x = function
    | None -> x
    | Some (_,y) -> Option.value ~default:y x
                    |> Int.min y
                    |> Option.return
                         
let min_score epos cache =
  Array.fold cache.(fst epos).(snd epos) ~init:None ~f:option_min
  |> Option.value_exn;;

let count_locs spos epos cache =
  let visited = Hash_set.create (module PosDir) in
  let rec helper = function
    | [] -> (visited
             |> Hash_set.to_list
             |> List.map ~f:(fun (x,_) -> x)
             |> List.dedup_and_sort ~compare:Pos.compare
             |> List.length)
    | (pos,exit_dir)::rest ->
      (if Hash_set.mem visited (pos,exit_dir)
       then helper rest
       else (Hash_set.add visited (pos,exit_dir) ;
             if Pos.equal spos pos
             then helper rest
             else helper (
                 let xs  = cache.(fst pos).(snd pos) in
                 let min = Array.foldi xs ~init:None ~f:(fun idx min -> function
                     | None -> min | Some (_,n) -> let in_dir = Direction.of_idx idx in
                       let n = n + (1000 * (diff exit_dir in_dir)) in
                       Option.value min ~default:n
                       |> Int.min n
                       |> Option.return)
                           |> Option.value_exn in
                 Array.foldi xs ~init:rest ~f:(fun idx acc -> function
                     | None -> acc
                     | Some (xs,n) ->
                       let in_dir = Direction.of_idx idx in
                       let n = n + (1000 * (diff exit_dir in_dir)) in
                       if n = min then List.fold_left ~init:acc ~f:(fun acc x -> (x,in_dir)::acc) xs else acc)))) in
  let res = helper [(epos,North)] in
  res
;;

let do_both () =
  let (map,epos,spos,dir) = (Sys.get_argv ()).(1)
                            |> read_input in
  let cache = search (map,epos,spos,dir) in
  let _part_a = cache
               |> min_score epos
               |> printf "Min score is %d\n" in
  let _part_b = cache
               |> count_locs spos epos
               |> printf "%d seats on optimal paths\n" in
  ()
;;

do_both ();;
