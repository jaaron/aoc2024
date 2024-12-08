open Core;;

let read_input f =
  let freqs = Hashtbl.create (module Char) in
  let add_freq_loc row col ls = Option.value ~default:[] ls
                                |> fun locs -> (row,col)::locs in
  let map = In_channel.read_lines f
            |> List.mapi ~f:(fun row s ->
                String.to_array s
                |> Array.mapi ~f:(fun col -> function
                    | '.' -> None
                    | x   -> (Hashtbl.update freqs x ~f:(add_freq_loc row col);
                              Some x)))
            |> Array.of_list
  in
  (freqs,map)
;;

(* Generate a boolean values output map to record antinodes *)
let make_outmap m =
  Array.init (Array.length m) ~f:(fun _ -> Array.create ~len:(Array.length m.(0)) false)
;;

(* Count all locations set (discovered antinodes) in the output map *)
let outmap_count_locs =
  Array.fold ~init:0 ~f:(fun acc ->
      Array.fold ~init:acc ~f:(fun acc -> function
          | true -> acc + 1
          | false -> acc))

(* Safe setter for the output map, returns true if the location was
   in-bounds false otherwise *)
let set_antinode outmap row col =
  try outmap.(row).(col) <- true; true
  with _ -> false;;

(* core of the solution for part A. Given two antenna locations,
   compute the (at most 2) distance-bounded antinodes based on the
   (delta-row, delta-column) between them. *)
let antinode_dist outmap (r0,c0) (r1,c1) =
  let dr = (r1 - r0) in
  let dc = (c1 - c0) in
  set_antinode outmap (r0 - dr) (c0 - dc) |> ignore;
  set_antinode outmap (r1 + dr) (c1 + dc) |> ignore
    
(* core of the solution for part B. Given two antenna locations
   compute all antinode locations by recursively applying the slope
   until we run off the map. *)
let antinode_line outmap (r0,c0) (r1,c1) =
  let rec iter_line ~f (r,c) (dr,dc) =
    (if f r c
     then iter_line ~f (r + dr, c + dc) (dr,dc)
     else ()) in
  let dr = (r1 - r0) in
  let dc = (c1 - c0) in
  iter_line ~f:(set_antinode outmap) (r0,c0) (dr,dc);
  iter_line ~f:(set_antinode outmap) (r0,c0) (-1*dr,-1*dc)

(* Iterate over all pairs of antennas of matching frequency applying
   the function f to each pair *)
let iter_antenna_pairs ~f =
  let rec helper f = function
    | loc0::rest -> List.iter ~f:(f loc0) rest;
        helper f rest
    | _ -> () in
  Hashtbl.iter ~f:(helper f)
;;

let part_a () =
  let f = (Sys.get_argv ()).(1) in
  let (freqs, map) = read_input f in
  let outmap = make_outmap map in
  let () = iter_antenna_pairs ~f:(antinode_dist outmap) freqs in
  outmap_count_locs outmap
  |> printf "There are %d antinodes\n"
;;

let part_b () =
  let f = (Sys.get_argv ()).(1) in
  let (freqs, map) = read_input f in
  let outmap = make_outmap map in
  let () = iter_antenna_pairs ~f:(antinode_line outmap) freqs in
  outmap_count_locs outmap
  |> printf "There are %d antinodes\n"
;;

part_a ();;
part_b ();;
