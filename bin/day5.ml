open Core;;
type rule = Before of (int * int);;

let read_input f =
  let rule_of_line l = String.lsplit2_exn ~on:'|' l
                       |> fun (x,y) -> Before (Int.of_string x, Int.of_string y) in
  let update_of_line l = String.split ~on:',' l
                         |> List.map ~f:Int.of_string in
  In_channel.with_file f
    ~f:(fun chan ->
        In_channel.fold_lines chan ~init:(`Rules,  [], [])
          ~f:(fun (state, rules, updates) l ->
              match state with
              | `Rules -> (if String.equal l ""
                           then (`Updates, rules, updates)
                           else (`Rules, (rule_of_line l)::rules, updates))
              | `Updates -> (`Updates, rules, (update_of_line l)::updates)))
  |> fun (_, rules, updates) -> (rules, updates)
;;

let update_valid rules update =
  let update_dict = List.mapi update ~f:(fun i n -> (n,i))
                    |> Hashtbl.of_alist_exn (module Int) in
  let pos x = Hashtbl.find update_dict x in
  List.for_all rules
    ~f:(fun (Before (pre,post)) ->
        match pos pre, pos post with
        | Some i, Some j -> i < j
        | _ -> true)

let get_middle_page update =
  let l = List.length update in
  List.nth_exn update (l/2)

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (rules, updates) -> List.filter ~f:(update_valid rules) updates)
  |> List.map ~f:get_middle_page
  |> List.fold ~f:(+) ~init:0
  |> printf "Well ordered: %d\n"
;;

let fix_invalid rules update =
  (* This isn't really a solution, we're just going to use `List.sort`
     with a custom comparator based on the rules. This could result in
     a violation if there are rules like `Before(a,b)`, `Before(b,c)`
     and the underlying sort routine compares `a` and `c` and
     concludes they're equal (since no ordering constraint exists on
     them). But our goal is just to be right enough...and this appears
     to work for the input given.
  *)
  let update_dict = List.mapi update ~f:(fun i n -> (n,i))
                    |> Hashtbl.of_alist_exn (module Int) in
  let pos x = Hashtbl.find update_dict x in
  let rules = List.filter rules ~f:(fun (Before (pre,post)) ->
      match pos pre, pos post with
      | Some _, Some _ -> true
      | _ -> false) in
  let compare a b = (* this could be too slow if the list of relevant
                       rules is quite long...could optimize to hashmap
                       of sets if needed. *)
    List.find_map rules ~f:(fun (Before (pre,post)) ->
      if (pre = a && post = b) then Some (-1)
      else if (post = a && pre = b) then Some (1)
      else None) |> Option.value ~default:0 in
  List.sort ~compare update
;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> fun (rules, updates) ->
  List.filter ~f:(fun u -> not (update_valid rules u)) updates
  |> List.map ~f:(fix_invalid rules)
  |> List.map ~f:get_middle_page
  |> List.fold ~f:(+) ~init:0
  |> printf "Fixed: %d\n"
;;

part_a ();;
part_b ();;
