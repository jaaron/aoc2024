open Core;;

type stone = Stone of int [@@deriving eq];;

let read_input f =
  In_channel.read_all f
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:(fun n -> Stone (Int.of_string n))
    
let nr_digits n = Int.to_string n |> String.length;;
let split_digits n = let s = Int.to_string n in
  let s0 = String.prefix s ((String.length s)/2) in
  let s1 = String.suffix s ((String.length s)/2) in
  (Int.of_string s0, Int.of_string s1)
;;

let step_stone acc = function
  | (Stone 0) -> (Stone 1)::acc
  | (Stone n) -> (if (nr_digits n) mod 2 = 0
                  then let (s1,s2) = split_digits n in
                    (Stone s2)::(Stone s1)::acc
                  else (Stone (n*2024))::acc)
;;

module IntPair = struct
  type t = (int * int) [@@deriving eq,sexp];;
  let hash (x,y) = x*y;;
  let compare (x0,y0) (x1,y1) =
    let c = Int.compare x0 x1 in
    if c = 0 then Int.compare y0 y1 else c
end;;

let rec count_stones_after =
  let cache = Hashtbl.create (module IntPair) in
  fun steps (Stone n) ->
    if steps = 0 then 1
    else match Hashtbl.find cache (steps,n) with
      | Some k -> k
      | None ->
        let k = (if n = 0
                 then count_stones_after (steps-1) (Stone 1)
                 else if (nr_digits n) mod 2 = 0
                 then let (s1,s2) = split_digits n in
                   let k0 = count_stones_after (steps-1) (Stone s1) in
                   let k1 = count_stones_after (steps-1) (Stone s2) in
                   k0 + k1
                 else count_stones_after (steps-1) (Stone (n*2024))) in
        Hashtbl.set cache ~key:(steps,n) ~data:k;
        k;;
                   
let step_all_stones l =
  List.fold_left ~init:[] ~f:step_stone l
  |> List.rev
;;

let rec multi_step_all_stones n l =
  if n = 0 then l
  else step_all_stones l
       |> multi_step_all_stones (n-1)
;;

let sum_stones =
  List.fold_left ~init:0 ~f:(fun acc (Stone s) -> acc + s)
;;
let _ = sum_stones;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> multi_step_all_stones 25
  |> List.length
  |> printf "After %d steps there are %d stones\n" 25
;;


let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> List.fold_left ~init:0 ~f:(fun acc stone ->
      acc + (count_stones_after 75 stone))
  |> printf "After 75 steps there are %d stones\n"
;;

part_a ();;
part_b ();;
