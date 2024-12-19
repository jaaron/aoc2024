open Core;;

module Color = struct
  type t = White | Blue | Black | Red | Green
  [@@deriving eq,compare,hash,sexp];;

  let string_of_color = function
    | White -> "w"
    | Blue  -> "u"
    | Black -> "b"
    | Red   -> "r"
    | Green -> "g"
  ;;
  let _ = string_of_color;;

  let color_of_char = function
    | 'w' -> White
    | 'u' -> Blue
    | 'b' -> Black
    | 'r' -> Red
    | 'g' -> Green
    | c -> failwith (sprintf "Unknown color '%c'" c)
  ;;
end;;
open Color;;

type pattern = Color.t list
[@@deriving eq,compare];;

let string_of_pattern p = 
  List.map ~f:string_of_color p
  |> String.concat ~sep:""
let _ = string_of_pattern;;

let pattern_of_string s =
  String.strip s
  |> String.to_list
  |> List.map ~f:color_of_char

module TowelTrie = struct
  type t =
  | Node of (pattern option * ((Color.t, t) Hashtbl.t));;

  let init () = Node (None, Hashtbl.create (module Color));;
  
  let insert trie towel = 
    let rec helper trie = function
      | []    -> (match trie with
          | None -> Node (Some towel, Hashtbl.create (module Color))
          | Some (Node (None, children)) -> Node (Some towel, children)
          | Some t -> t)
      | c::cs -> match trie with
        | None -> Node (None, Hashtbl.of_alist_exn (module Color) [c, helper None cs])
        | Some (Node (x, children)) ->
          Hashtbl.update children c ~f:(fun x -> helper x cs);
          Node (x, children) in
    helper (Some trie) towel;;

  let rec fold ~init ~f = function
    | Node (towel_opt, children) ->
      Hashtbl.fold ~init:(Option.map ~f:(f init) towel_opt
                          |> Option.value ~default:init)
        ~f:(fun ~key:_ ~data init -> fold ~init ~f data) children
  ;;
  let _ = fold;;
  
  let find_all trie pat =
    let rec helper acc trie = function
        | []    -> (match trie with | Node (Some towel, _) -> ([],towel)::acc
                                    | _ -> acc)
        | (p::ps) as pat ->
            (match trie with
             | Node (towel, children) -> 
                 let acc = Option.map towel ~f:(fun towel -> (pat,towel)::acc)
                           |> Option.value ~default:acc in
                 match Hashtbl.find children p with
                 | Some trie -> helper acc trie ps
                 | None      -> acc)
    in
    helper [] trie pat
  ;;
end;;


let read_input f =
  In_channel.with_file f ~f:(fun chan ->
      let towels = In_channel.input_line chan
                   |> Option.value_exn
                   |> String.split ~on:','
                   |> List.fold_left ~init:(TowelTrie.init ())
                     ~f:(fun trie s -> pattern_of_string s
                                       |> TowelTrie.insert trie)  in
      let _  = In_channel.input_line chan in
      (towels,
       In_channel.fold_lines chan ~init:[]
         ~f:(fun acc l ->
             (pattern_of_string l)::acc)
       |> List.rev))
;;

let check_pattern cache towels pat =
    let rec helper pat =
        match pat with
        | [] -> true
        | pat ->
            let patstr = string_of_pattern pat in
            match Hashtbl.find cache patstr with
            | Some b -> b
            | None -> 
                let cs  = TowelTrie.find_all towels pat in
                let hds = List.map cs ~f:fst in
                let res = List.exists hds ~f:(fun c -> helper c) in
                Hashtbl.set cache ~key:patstr ~data:res;
                res in
    helper pat;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (towels,pats) ->
         let cache = Hashtbl.create (module String) in
         List.fold_left pats ~init:0 ~f:(fun acc pat ->
                                            Out_channel.flush stdout;
                                            if check_pattern cache towels pat
                                            then acc+1 else acc))
  |> printf "Matched %d patterns\n"
;;

part_a ();;

let count_arrangements cache towels pat =
    let rec helper pat =
        match pat with
        | [] -> 1
        | pat -> let patstr = string_of_pattern pat in
            match Hashtbl.find cache patstr with
            | Some n -> n
            | None  ->
                let cs      = TowelTrie.find_all towels pat in
                let cnt     = List.fold_left cs ~init:0 ~f:(fun cnt (pat,_) -> cnt + (helper pat)) in
                Hashtbl.set cache ~key:patstr ~data:cnt ;
                cnt in
    helper pat
;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (towels,pats) ->
         let cache = Hashtbl.create (module String) in
         List.fold_left pats ~init:0 ~f:(fun acc pat ->
                                            Out_channel.flush stdout;
                                            acc + (count_arrangements cache towels pat)))
  |> printf "Total sequences: %d\n"
;;

part_b ();;
