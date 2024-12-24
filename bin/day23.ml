open Core;;

let read_input f =
    let res = Hashtbl.create (module String) in
    In_channel.read_lines f
    |> List.iter ~f:(fun s ->
                       let (l,r) = String.lsplit2_exn ~on:'-' s in
                       let (l,r) = if String.compare l r < 0 then (l,r) else (r,l) in
                       Hashtbl.update res l
                           ~f:(function Some s -> Hash_set.add s r; s
                                      | None   ->
                                          let s = Hash_set.create (module String) in
                                          Hash_set.add s r;
                                          s));
    res;;

let check_neighbor map l r =
    let (l,r) = if String.compare l r < 0 then (l,r) else (r,l) in
    match Hashtbl.find map l with
    | None -> false
    | Some nbrs -> Hash_set.mem nbrs r
;;

let all_pairs s =
    let rec helper acc = function
        | [] -> acc
        | x::xs -> helper (List.fold_left ~init:acc ~f:(fun acc y -> (x,y)::acc) xs) xs
    in
    Hash_set.to_list s
    |> helper []
;;

let get_throuples map k =
    match Hashtbl.find map k with
    | None -> []
    | Some nbrs ->
        all_pairs nbrs
        |> List.filter_map ~f:(fun (x,y) ->
                                  if check_neighbor map x y
                                  then Some (k,x,y) else None)
        
let get_all_throuples map =
    Hashtbl.keys map
    |> List.map ~f:(fun key -> get_throuples map key)
    |> List.concat

let part_a () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> get_all_throuples
    |> List.filter ~f:(fun (x,y,z) ->
                          (String.get x 0 |> Char.equal 't') ||
                          (String.get y 0 |> Char.equal 't') ||
                          (String.get z 0 |> Char.equal 't'))
    |> List.length
    |> printf "%d throuples with 't'\n"
;;

part_a ();;

let rec all_subsets = function
    | [] -> [[]]
    | x::xs ->
        let r = all_subsets xs in
        List.fold_left ~init:r ~f:(fun acc ys -> (x::ys)::acc) r
;;

let rec check_clique map = function
    | []    -> true
    | x::xs ->
        List.for_all xs ~f:(check_neighbor map x) &&
        check_clique map xs;;

let largest_clique_with map k =
    match Hashtbl.find map k with
    | None -> []
    | Some nbrs ->
        Hash_set.to_list nbrs
        |> all_subsets
        |> List.sort ~compare:(fun c1 c2 -> -1*(Int.compare (List.length c1) (List.length c2)))
        |> List.find ~f:(check_clique map)
        |> Option.value ~default:[]
        |> fun c -> (k::c)
;;

let largest_clique map =
    Hashtbl.keys map
    |> List.fold_left ~init:[]
           ~f:(fun best k ->
                  let clique = largest_clique_with map k in
                  if List.length clique > List.length best
                  then clique
                  else best)
    |> List.sort ~compare:String.compare

let part_b () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> largest_clique
    |> String.concat ~sep:","
    |> printf "Password is %s\n"
;;


part_b ();;
