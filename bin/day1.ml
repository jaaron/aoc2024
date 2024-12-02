open Core;;

let read_input x = 
  In_channel.read_lines x
  |> List.map ~f:(fun l ->
      String.lsplit2_exn ~on:' ' l
      |> fun (x,y) -> (String.strip x
                       |> Int.of_string,
                       String.strip y
                       |> Int.of_string))
    
let part_1 () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> List.unzip
  |> fun (xs, ys) ->
  let xs = List.sort ~compare:Int.compare xs in
  let ys = List.sort ~compare:Int.compare ys in
  List.map2_exn xs ys ~f:(fun x y -> Int.abs (x - y))
  |> List.fold_left ~init:0 ~f:(+)
  |> printf "Total distance is: %d\n"
;;

let part_2 () =
  (Sys.get_argv()).(1)
  |> read_input
  |> List.unzip
  |> fun (xs, ys) ->
  let xs = List.sort ~compare:Int.compare xs in
  let ys = List.sort ~compare:Int.compare ys in
  let rec count_head acc a = function
    | [] -> acc
    | b::cs -> (if a = b
                then count_head (acc + 1) a cs
                else acc) in
  let rec helper tot = function
    | [] -> fun _ -> tot
    | x::xs ->
      fun ys ->
        let cnt_left  = count_head 1 x xs in
        let ys = List.drop_while ~f:(fun y -> y < x) ys in
        let cnt_right = count_head 0 x ys in
        helper
          (tot + (x * cnt_left * cnt_right))
          (List.drop xs (cnt_left - 1))
          (List.drop ys cnt_right) in
  helper 0 xs ys
  |> printf "Total similarity is %d\n" ;;
    
part_1 ();;
part_2 ();;

