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
    let (fspecs,free) = Array.foldi m ~init:(true,0,0,[],[])
                            ~f:(fun idx (inuse,start,fid,fids,free) -> function
                                   | Some fid' -> (if inuse
                                                 then (if fid = fid'
                                                       then (inuse,start,fid,fids,free)
                                                       else (inuse,idx,fid',(start,idx - start,fid)::fids, free))
                                                 else (true,idx,fid',fids,(start,idx-start)::free))
                                   | None -> (if inuse
                                              then (false,idx,fid,(start,idx-start,fid)::fids, free)
                                              else (false,start,fid,fids,free)))
                        |> fun (inuse,start,fid,fids,free) ->
                        let fids = if inuse then (start, (Array.length m) - start, fid)::fids else fids in
                        (fids,List.rev free) in
    let rec set start len fid =
        if len <= 0
        then ()
        else (
            m.(start) <- Some fid;
            set (start + 1) (len-1) fid
        ) in
    let rec clear start len =
        if len <=  0
        then ()
        else (
            m.(start) <- None;
            clear (start + 1) (len-1)
        ) in
    let rec insert (start,len) acc = function
        | [] -> List.rev ((start,len)::acc)
        | (x,n)::[] -> (
                if start + len = x
                then List.rev ((start, len + n)::acc)
                else if x + n = start
                then List.rev ((x, len+n)::acc)
                else if start < x
                then List.rev ((x,n)::(start,len)::acc)
                else List.rev ((start,len)::(x,n)::acc))
        | (x,n)::(y,m)::rest ->
            if start + len < x
            then List.rev_append ((start,len)::acc) ((x,n)::(y,m)::rest)
            else if start + len = x
            then List.rev_append ((start,len+n)::acc) ((y,m)::rest)
            else if start = x + n
            then (if start + len = y
                  then List.rev_append ((x,n + len + m)::acc ) rest
                  else List.rev_append ((x,n+len)::acc) ((y,m)::rest))
            else insert (start,len) ((x,n)::acc) ((y,m)::rest) in
    let rec relocate (start,len,fid) acc = function
        | [] -> List.rev acc
        | (f_start,f_len)::free ->
            if f_start > start
            then List.rev acc
            else if f_len < len
            then relocate (start,len,fid) ((f_start,f_len)::acc) free
            else (
                set f_start len fid;
                clear start len;
                let free = insert (start,len) [] free in
                let acc  = if f_len = len then acc else (f_start + len, f_len - len)::acc in
                List.rev_append acc free
            ) in
    let rec helper free = function
        | [] -> m
        | fspec::fs -> helper (relocate fspec [] free) fs in
    helper free fspecs
;;
                                                 
let part_b () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> filewise_compact
    |> checksum
    |> printf "Checksum is %d\n"
;;

part_a ();;
part_b ();;
