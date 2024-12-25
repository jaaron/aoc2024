open Core;;

let read_input f =
  let read_lock_or_key chan =
    let l1 = In_channel.input_line_exn chan in
    let l2 = In_channel.input_line_exn chan in
    let l3 = In_channel.input_line_exn chan in
    let l4 = In_channel.input_line_exn chan in
    let l5 = In_channel.input_line_exn chan in
    let l6 = In_channel.input_line_exn chan in
    let l7 = In_channel.input_line_exn chan in
    let _  = try In_channel.input_line_exn chan with _ -> "" in
    let is_lock = String.equal l1 "#####" in
    (List.fold_left ~init:(Array.create ~len:5 0)
       ~f:(fun tumblers l ->
           String.iteri l ~f:(fun i -> function
               | '#' -> tumblers.(i) <- tumblers.(i)+1
               | _   -> ());
           tumblers)
       (if is_lock
        then [l2;l3;l4;l5;l6;l7]
        else [l6;l5;l4;l3;l2;l1]))
    |>  fun x -> if is_lock then `Lock x else `Key x in
  let rec read_all (locks,keys) chan =
    try
      match read_lock_or_key chan with
      | `Lock x -> read_all (x::locks,keys) chan
      | `Key x  -> read_all (locks,x::keys) chan
    with _ -> (locks,keys) in
  In_channel.with_file f ~f:(read_all ([],[]))
      
let key_fits ~lock ~key =
  let res = Array.for_alli lock ~f:(fun i d -> (d + key.(i)) < 6) in
  res

let rec check_all acc locks = function
  | [] -> acc
  | (key::keys) ->
    let rec helper acc = function
      | [] -> check_all acc locks keys
      | (lock::locks) ->
        helper (if key_fits ~lock ~key
                then (acc+1) else acc) locks
    in
    helper acc locks
      ;;

let () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (locks,keys) -> check_all 0 locks keys)
  |> printf "%d valid lock/key pairs\n"
    
