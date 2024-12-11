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

let solve n =
  (Sys.get_argv ()).(1)
  |> read_input
  |> List.fold_left ~init:0 ~f:(fun acc stone ->
      acc + (count_stones_after n stone))
  |> printf "After %d steps there are %d stones\n" n
;;

let part_a () = solve 25;;
let part_b () = solve 75;;

part_a ();;
part_b ();;
