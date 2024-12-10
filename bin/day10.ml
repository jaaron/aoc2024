open Core;;

let read_input f =
  let int_of_digit c = let n = (Char.to_int c) - 48 in
    if n < 0 || n > 9 then failwith "Character out of range"
    else n in
  In_channel.read_lines f
  |> List.map ~f:(fun l -> String.to_list l
                           |> List.map ~f:int_of_digit
                           |> Array.of_list)
  |> Array.of_list
;;

let get m x y    = try m.(x).(y) |> Option.return with _ -> None;;
let set m x y v  = try m.(x).(y) <- v with _ -> ();;
let rows m       = Array.length m;;
let cols m       = Array.length m.(0);;
let init_map m v = Array.init ~f:(fun _ -> Array.create ~len:(cols m) v) (rows m);;

let neighbors_at_height ~height m x y =
  List.filter_map ~f:(fun (dx,dy) ->
      let x = x + dx in
      let y = y + dy in
      match get m x y with
      | None -> None
      | Some h -> if height = h then Some (x,y) else None)
    [(-1,0); (1,0); (0,-1); (0,1)]
;;

let rec visit_peaks ~f m = function
  | [] -> ()
  | (x,y)::locs ->
    match get m x y with
    | None   -> visit_peaks ~f m locs
    | Some h -> if h >= 9
      then (f x y;
            visit_peaks ~f m locs)
      else visit_peaks ~f m
          ((neighbors_at_height ~height:(h+1) m x y)@locs)

let score_trailhead m loc =
  let rps = init_map m false in
  let ()  = visit_peaks ~f:(fun x y -> set rps x y true) m [loc] in
  Array.fold ~init:0 ~f:(fun cnt ->
      Array.fold ~init:cnt ~f:(fun cnt -> function
          | true -> cnt + 1
          | false -> cnt)) rps
;;

let sum_all_trailheads ~f m =
  Array.foldi ~init:0 ~f:(fun x cnt ->
      Array.foldi ~init:cnt ~f:(fun y cnt -> function
          | 0 -> cnt + (f m (x,y))
          | _ -> cnt)) m
;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> sum_all_trailheads ~f:score_trailhead
  |> printf "Sum of trailhead scores is %d\n"
;;

let rate_trailhead m loc =
  let rps = init_map m 0 in
  let ()  = visit_peaks ~f:(fun x y -> set rps x y (rps.(x).(y) + 1)) m [loc] in
  Array.fold ~init:0 ~f:(fun cnt ->
      Array.fold ~init:cnt ~f:(+)) rps
;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> sum_all_trailheads ~f:rate_trailhead
  |> printf "Sum of trailhead ratings is %d\n"
;;

part_a ();;
part_b ();;

