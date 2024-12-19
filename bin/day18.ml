open Core;;
let read_input f =
  In_channel.read_lines f
  |> List.map ~f:(fun l ->
      String.lsplit2_exn ~on:',' l
      |> fun (x,y) -> (Int.of_string x,
                       Int.of_string y))
;;

type cell = Safe | Corrupt of int
let init_map ~count ~max_x ~max_y input =
  let map = Array.init max_x ~f:(fun _ -> Array.create ~len:max_y Safe) in
  let rec helper n = function
    | [] -> ()
    | (x,y)::input -> 
      if n >= count
      then ()
      else ((* printf "Setting %d,%d\n" x y; *)
            map.(x).(y) <- Corrupt n;
            helper (n+1) input) in
  helper 0 input;
  map
;;

let pmap map =
  Array.iter map
    ~f:(fun row ->
        Array.iter row ~f:(function
            | Corrupt _ -> printf "#"
            | Safe -> printf ".");
        Out_channel.newline stdout)
;;

(* let search map = *)
(*   let max_x = Array.length map in *)
(*   let max_y = Array.length map.(0) in *)
(*   let visited = Array.init max_x ~f:(fun _ -> Array.create ~len:max_y None) in *)
(*   let pvisited () = Array.iteri visited *)
(*       ~f:(fun x row -> Array.iteri row ~f:(fun y -> function *)
(*           | None -> (match map.(x).(y) with *)
(*               | Safe -> Out_channel.output_char stdout '.' *)
(*               | Corrupt _ -> Out_channel.output_char stdout '#') *)
(*           | Some _ -> Out_channel.output_char stdout 'O'); *)
(*           Out_channel.newline stdout) in *)
(*   let _ = pvisited in *)
(*   let visit ~q steps x y = *)
(*     visited.(x).(y) <- Some steps; *)
(*     q@(List.fold_left ~init:[] ~f:(fun acc (dx,dy) -> *)
(*         let x = x + dx in *)
(*         let y = y + dy in *)
(*         if (x >= 0 && x < max_x && y >= 0 && y < max_y) *)
(*         then match map.(x).(y) with *)
(*           | Safe -> (\* printf "\t%d,%d is safe\n" x y; *\) ((x,y),steps+1)::acc *)
(*           | Corrupt _ -> acc *)
(*         else acc) *)
(*       [(1,0);(0,1);(-1,0);(0,-1)]) in *)
(*   let rec helper i  = function *)
(*     | [] -> () *)
(*     | ((x,y),steps)::q -> *)
(*       if i mod 100000 = 0 *)
(*       then (printf "i = %10d: Visiting (%d,%d)\n" i x y; *)
(*             pvisited ()); *)
(*       if (\* short circuit if we've already visited this location with fewer steps *\) *)
(*         (Option.map visited.(x).(y) ~f:(fun best -> best < steps) *)
(*          |> Option.value ~default:false) || *)
(*         (\* or we've already found the goal with fewer steps *\) *)
(*         (Option.map visited.(max_x-1).(max_y-1) ~f:(fun best -> best < steps) *)
(*          |> Option.value ~default:false) *)
(*       then helper (i+1) q *)
(*       else ( *)
(*         Out_channel.flush stdout; *)
(*         if (x = max_x-1) && (y = max_y-1) *)
(*         then (visited.(x).(y) <- Some steps; *)
(*               Out_channel.printf "Found solution %d (q = %d)" steps (List.length q); *)
(*               Out_channel.flush stdout; *)
(*               helper (i+1) q) *)
(*         else *)
(*           match visited.(x).(y) with *)
(*           | None -> helper (i+1) (visit ~q steps x y) *)
(*           | Some min_steps -> (if min_steps < steps *)
(*                                then helper (i+1) q *)
(*                                else helper (i+1) (visit ~q steps x y))) *)
(*   in *)
(*   helper 0 [((0,0),0)];       *)
(*   visited.(max_x-1).(max_y-1) *)
(* ;; *)

let search map =
  let max_x = Array.length map in
  let max_y = Array.length map.(0) in
  let visited = Array.init max_x ~f:(fun _ -> Array.create ~len:max_y None) in
  let pvisited () = Array.iteri visited
      ~f:(fun x row -> Array.iteri row ~f:(fun y -> function
          | None -> (match map.(x).(y) with
              | Safe -> Out_channel.output_char stdout '.'
              | Corrupt _ -> Out_channel.output_char stdout '#')
          | Some _ -> Out_channel.output_char stdout 'O');
          Out_channel.newline stdout) in
  let _ = pvisited in
  let rec helper steps frontier =
    List.iter frontier ~f:(fun (x,y) -> visited.(x).(y) <- Some steps);
    List.fold_left frontier ~init:[]
      ~f:(fun acc (x,y) ->
          List.fold_left ~init:acc ~f:(fun acc (dx,dy) ->
              let x = x + dx in
              let y = y + dy in
              if (x >= 0) && (x < max_x) && (y >= 0) && (y < max_y)
              then
                match map.(x).(y),visited.(x).(y) with
                | Safe,None -> (x,y)::acc
                | _         -> acc
              else acc) [(1,0);(-1,0);(0,1);(0,-1)])
    |> List.dedup_and_sort ~compare:(fun (x1,y1) (x2,y2) ->
        let c = Int.compare x1 x2 in if c = 0 then Int.compare y1 y2 else c)
    |> function
    | [] -> ()
    | frontier -> helper (steps+1) frontier
        in
  helper 0 [(0,0)];      
  visited.(max_x-1).(max_y-1)
;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> init_map ~count:1024 ~max_x:71 ~max_y:71
  |> (fun map -> pmap map;map)
  |> search
  |> Option.value_exn
  |> printf "Min steps is %d\n"
;;

part_a ();;

let binsearch inputs =
  let rec helper min_n max_n =
    if min_n = max_n
    then (pmap (init_map ~count:(max_n-1) ~max_x:71 ~max_y:71 inputs);
          List.nth_exn inputs (max_n-1))
    else if max_n = min_n+1
    then helper max_n max_n
    else
      let n = (min_n + ((max_n - min_n)/2)) in
      let map = init_map ~count:n ~max_x:71 ~max_y:71 inputs in
      match search map with
      | None -> helper min_n n
      | Some _ -> helper n max_n
  in
  helper 1024 (List.length inputs)

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> binsearch
  |> fun (x,y) -> printf "Path is blocked at %d,%d\n" x y
;;
part_b ();;
