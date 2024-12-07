open Core;;

let parse_input_line l =
  let (lhs,rhs) = String.lsplit2_exn ~on:':' l in
  (Int.of_string lhs,
   String.strip rhs
   |> String.split ~on:' '
   |> List.map ~f:Int.of_string)
;;

(* core of the solution given the running calibration total (acc) and
   a calibration equation expressed as (lhs, xs) iterate over the
   righthand side (xs) applying the function f to each term to
   generate candidate sub-totals for this equation.

   The function f should take the left hand side, the current subtotal
   and the new term and return a list of new candidate subtotals after
   consuming the term (derived by applying the valid operators to the
   subtotal and new term).
*)
let check_equation ~f acc lhs xs =
  let rec helper tots = function
    | []    -> if List.exists ~f:(fun tot -> tot = lhs) tots then acc + lhs else acc
    | x::xs -> helper (List.fold_left tots ~init:[]
                         ~f:(fun acc tot -> (f lhs tot x)@acc)) xs
  in
  match xs with
  | []    -> acc
  | x::xs -> helper [x] xs
;;

let check_line ~f acc l =
  if String.length l = 0 then acc
  else
    let (lhs,xs) = parse_input_line l in
    check_equation acc ~f lhs xs
;;


(* helper function, prepend element a to the list l iff (f a) is true. *)
let prepend_if ~f a l = if f a then a::l else l;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> In_channel.with_file ~f:(fun chan ->
      In_channel.fold_lines chan ~init:0
        ~f:(check_line ~f:(fun lhs tot x ->
            let inbound = fun x -> x <= lhs in
            prepend_if ~f:inbound (tot+x) []
            |> prepend_if ~f:inbound (tot*x))))
  |> printf "Calibration total is: %d\n"
;;

let do_concat l r =
  Int.to_string r
  |> String.length
  |> Int.pow 10
  |> fun x -> (l * x) + r

let part_b () =
  (Sys.get_argv ()).(1)
  |> In_channel.with_file ~f:(fun chan ->
      In_channel.fold_lines chan ~init:0
        ~f:(check_line ~f:(fun lhs tot x ->
            let inbound = fun x -> x <= lhs in
            prepend_if ~f:inbound (tot+x) []
            |> prepend_if ~f:inbound (tot*x)
            |> prepend_if ~f:inbound (do_concat tot x))))
  |> printf "Calibration total is: %d\n"
;;


part_a ();;
part_b ();;
