open Core;;
let read_input f =
    let s = In_channel.read_all f |> String.strip in
    let ns = String.to_list s |> List.map ~f:(fun c -> (Char.to_int c) - 48) in
    let () = List.iter ns ~f:(fun n -> if n < 0 || n > 9 then failwith (sprintf "Out of range %d" n)) in
    let len = List.fold_left ns ~init:0 ~f:(+) in
    let expanded = Array.create ~len None in
    List.fold_left ns ~init:(0,0,false)
        ~f:(fun (init,fid,is_free) cnt ->
               (if not is_free
                then for i = init to init + cnt -1 do
                        expanded.(i) <- Some fid
                    done);
               (init+cnt, (if is_free then fid else fid+1), not is_free))
    |> ignore;
    expanded
;;

let print_diskmap m =
    Array.iter ~f:(function
                      | None -> printf "."
                      | Some fid -> printf "%d" fid) m;
    Out_channel.(newline stdout);;
;;

let _ = print_diskmap;;

let blockwise_compact m =
    let rec helper wh rh =
        if wh >= rh then ()
        else
            match (m.(wh),m.(rh)) with
            | (None, Some fid) -> (m.(wh) <- Some fid;
                                   m.(rh) <- None;
                                   helper (wh+1) (rh-1))
            | (None, None)     -> helper wh (rh-1)
            | (Some _, None)   -> helper (wh+1) (rh-1)
            | (Some _, Some _) -> helper (wh+1) rh
    in
    helper 0 ((Array.length m)-1);
    m
;;

let checksum m =
    Array.foldi m ~init:0 ~f:(fun i acc -> function
                                 | None -> acc
                                 | Some fid -> acc + (fid*i))
;;

let part_a () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> blockwise_compact
    |> checksum
    |> printf "Checksum is %d\n"
;;

let filewise_compact m =
    let rec check_space cnt start =
        if cnt = 0
        then true
        else if start >= Array.length m
        then false
        else match m.(start) with
        | Some _ -> false
        | None   -> check_space (cnt-1) (start + 1) in
    let rec find_space ~cnt ~max start =
        if start >= max then None
        else
        match m.(start) with
        | Some _ -> find_space ~cnt ~max (start + 1)
        | None   -> (if check_space cnt start
                     then Some start
                     else find_space ~cnt ~max (start + 1)) in
    let rec relocate start_loc end_loc space_start =
        if start_loc > end_loc then ()
        else (
            m.(space_start) <- m.(start_loc);
            m.(start_loc) <- None;
            relocate (start_loc + 1) end_loc (space_start + 1)
        ) in
    let rec helper cur head =
        if head <= 0 then m
        else
        match cur,m.(head) with
        | None, None             -> helper None (head-1)
        | None, Some _           -> helper (Some head) (head-1)
        | Some end_loc, None     -> (find_space ~cnt:(end_loc - head) ~max:head 0
                                     |> Option.map ~f:(relocate (head+1) end_loc)
                                     |> Option.value ~default:();
                                     helper None head)
        | Some end_loc, Some fid -> (if (m.(end_loc) |> Option.value_exn) = fid
                                     then helper cur (head-1)
                                     else (find_space ~cnt:(end_loc - head) ~max:head 0
                                           |> Option.map ~f:(relocate (head+1) end_loc)
                                           |> Option.value ~default:();
                                           helper (Some head) (head-1)))
    in
    helper None ((Array.length m)-1)
                                                 
let part_b () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> filewise_compact
    |> checksum
    |> printf "Checksum is %d\n"
;;

part_a ();;
part_b ();;
