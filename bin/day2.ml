open Core;;

let read_input f =
  In_channel.read_lines f
  |> List.map ~f:(fun l -> String.split ~on:' ' l
                           |> List.map ~f:Int.of_string)

type direction = Asc | Desc
[@@deriving eq];;

let max_safe_delta = 3;;

(** is_safe_report addresses parts A and B of the challenge.

 recover function takes four parameters: 
     `all_prev`, `prev_lvl`, `lvl`, abd `levels` 
 where:
 * the `List.rev_append (prev_lvl::all_prev) (lvl::levels)` 
   is the original report
 * and a fault was detected validating `lvl` against `prev_lvl`.
*) 
let is_safe_report ?(recover = (fun _ _ _ _ -> false)) = function
  | [] -> true
  | lvl::levels ->
    let rec helper dir all_prev prev_lvl = function
      | [] -> true
      | lvl::levels ->
        let delta = Int.abs (prev_lvl - lvl) in
        let sign  = if prev_lvl > lvl then Desc else Asc in
        let same_dir = Option.map dir ~f:(fun d -> equal_direction d sign)
                       |> Option.value ~default:true in
        if delta < 1 || delta > max_safe_delta || (not same_dir)
        then (recover all_prev prev_lvl lvl levels)
        else helper (Some sign) (prev_lvl::all_prev) lvl levels
    in
    helper None [] lvl levels

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> List.fold_left ~init:0
    ~f:(fun nsafe -> fun report ->
        if is_safe_report report
        then nsafe + 1
        else nsafe)
  |> printf "Total safe is %d\n"
;;

let part_b () =
  let recover all_prev prev_lvl lvl levels =
    ((* recover by dropping the level before the fault *)
      is_safe_report (List.rev_append all_prev (lvl::levels)) ||
      (* recovery by dropping the level that triggerd the fault *)
      is_safe_report (List.rev_append (prev_lvl::all_prev) levels) ||
      (* if we've only seen two previous levels, drop the very
           first one in case the direction switched anomalously
           between first and second. *)
      ((List.length all_prev) = 1 && is_safe_report (prev_lvl::lvl::levels))
    ) in
  (Sys.get_argv ()).(1)
  |> read_input
  |> List.fold_left ~init:0
    ~f:(fun nsafe -> fun report ->
        if is_safe_report ~recover report
        then nsafe + 1
        else nsafe)
  |> printf "Total safe is %d\n"
;;


part_a ();;
part_b ();;
