open Core;;

type action = Left | Right | Up | Down | Act
[@@deriving eq,compare,hash,sexp];;

module type Pad = sig
    type t [@@deriving eq,compare,hash,sexp];;
    val coords : t -> (int * int);;
    val of_coords : (int * int) -> t;;
    val to_string : t -> string;;
end;;
    
module Numpad = struct
    type t = Seven
           | Eight
           | Nine
           | Four
           | Five
           | Six
           | One
           | Two
           | Three
           | Zero
           | Activate
    [@@deriving eq,compare,hash,sexp];;

    let coords = function
        | Seven    -> (0,0)
        | Eight    -> (0,1)
        | Nine     -> (0,2)
        | Four     -> (1,0)
        | Five     -> (1,1)
        | Six      -> (1,2)
        | One      -> (2,0)
        | Two      -> (2,1)
        | Three    -> (2,2)
        | Zero     -> (3,1)
        | Activate -> (3,2)
    let of_coords = function
        | (0,0) -> Seven
        | (0,1) -> Eight
        | (0,2) -> Nine
        | (1,0) -> Four
        | (1,1) -> Five
        | (1,2) -> Six
        | (2,0) -> One
        | (2,1) -> Two
        | (2,2) -> Three
        | (3,1) -> Zero
        | (3,2) -> Activate
        | (x,y) -> failwith (sprintf "Invalid coords (%d,%d)" x y)
                       
    let to_string = function
        | Seven -> "7"
        | Eight -> "8"
        | Nine  -> "9"
        | Four  -> "4"
        | Five  -> "5"
        | Six   -> "6"
        | One   -> "1"
        | Two   -> "2"
        | Three -> "3"
        | Zero  -> "0"
        | Activate -> "A"
    ;;
end;;

module Dirpad = struct
    type t = action
    [@@deriving eq,compare,hash,sexp];;

    let coords = function
        | Up    -> (0,1)
        | Act   -> (0,2)
        | Left  -> (1,0)
        | Down  -> (1,1)
        | Right -> (1,2)
                   
    let of_coords = function
        | (0,1) -> Up
        | (0,2) -> Act
        | (1,0) -> Left
        | (1,1) -> Down
        | (1,2) -> Right
        | (r,c) -> failwith (sprintf "Invalid coords (%d,%d)" r c)
                       
    let to_string = function
        | Left -> "<"
        | Right -> ">"
        | Up -> "^"
        | Down -> "v"
        | Act -> "A"
    ;;

    let of_char = function
        | '<' -> Left
        | '>' -> Right
        | 'v' -> Down
        | '^' -> Up
        | 'A' -> Act
        | _   -> failwith "Invalid direction";;
    
    let list_of_string s =
          String.to_list s |> List.map ~f:of_char;;

    let _ = list_of_string;;
end;;

module Choice = struct
  type 'a t = OneOf of 'a list;;
  let map ~f (OneOf xs) = OneOf (List.map ~f xs);;
  let fold ~init ~f (OneOf xs) = List.fold ~init ~f xs;;
  let _ = fold;;
  
  let cross ~f (OneOf xs) (OneOf ys) =
    OneOf (List.concat_map xs ~f:(fun x ->
        List.map ys ~f:(fun y -> f x y)))
  ;;
  let _ = cross;;
  
  let min ~compare = function
    | OneOf [] -> failwith "No choices"
    | OneOf (x::xs) -> List.fold_left xs ~init:x ~f:(fun m x ->
        if compare m x <= 0 then m else x);;
end;;

module Robot (M : Pad) = struct        
    let left x  = M.coords x
                  |> (fun (r,c) -> (r,c-1))
                  |> M.of_coords
    let right x = M.coords x
                  |> (fun (r,c) -> (r,c+1))
                  |> M.of_coords    
    let up x    = M.coords x
                  |> (fun (r,c) -> (r-1,c))
                  |> M.of_coords
    let down x  = M.coords x
                  |> (fun (r,c) -> (r+1,c))
                  |> M.of_coords

    let move = function
        | Left  -> left
        | Right -> right
        | Up    -> up
        | Down  -> down
        | Act   -> fun s -> printf "%s" (M.to_string s) ; s
    ;;
    
    let _ = move;;
    
    let path from to_ =
        let (r0,c0) = M.coords from in
        let (r1,c1) = M.coords to_ in
        let v = List.init (abs (c1-c0)) ~f:(fun _ -> (if c0 < c1 then Right else Left)) in
        let h = List.init (abs (r1-r0)) ~f:(fun _ -> if r0 < r1 then Down else Up) in
        let res = [] in
        let res = try let _ = M.of_coords (r1,c0) in (h@v)::res with _ -> res in
        let res = try let _ = M.of_coords (r0,c1) in (v@h)::res with _ -> res in
        Choice.OneOf res

    let multipath =        
        let rec helper from = function
          | [] -> []
          | dest::rest ->
            let p0    = path from dest in
            let prest = helper dest rest in
            p0::prest
        in function
            | [] -> []
            | start::dests -> helper start dests
end


let code_of_string s =
    String.fold s ~init:[]
        ~f:(fun acc c ->
               let open Numpad in
               (match c with
                | '0' -> Zero
                | '1' -> One
                | '2' -> Two
                | '3' -> Three
                | '4' -> Four
                | '5' -> Five
                | '6' -> Six
                | '7' -> Seven
                | '8' -> Eight
                | '9' -> Nine
                | 'A' -> Activate
                | _ -> failwith "Invalid character")::acc)
    |> List.rev;;

module NumpadBot = Robot (Numpad);;
module DirpadBot = Robot (Dirpad);;

let rec ppath = function
    | [] -> Out_channel.newline stdout
    | x::xs -> printf "%s" (Dirpad.to_string x); ppath xs
let _ = ppath;;

let min_length_n_bots ~n goal =
  let cache = Hashtbl.create (module struct type t = (int * action list) [@@deriving eq,compare,sexp,hash] end) in
  let rec helper n g =
    if n = 0 then List.length g
    else (
      match Hashtbl.find cache (n,g) with
      | Some n -> n
      | None ->
        Out_channel.flush stdout;
        let open DirpadBot in
        let paths = multipath (Act::g) in
        let res = List.fold_left ~init:0 paths
            ~f:(fun acc choices ->
                (Choice.map choices ~f:(fun path -> helper (n-1) (path@[Act]))
                 |> Choice.min ~compare:Int.compare)+acc) in
        Hashtbl.set cache ~key:(n,g) ~data:res;
        res
    )
  in

  NumpadBot.multipath goal
  |> List.fold_left ~init:0 ~f:(fun acc choices ->
      Choice.map choices ~f:(fun path -> helper n (path@[Act]))
      |> Choice.min ~compare:Int.compare
      |> fun x -> x + acc)

let solve ~n goal_strs =
  let rec helper acc = function
    | [] -> acc
    | goal::goals ->
      helper (((min_length_n_bots ~n
                  (Numpad.Activate::(code_of_string goal))
               )*
               (Int.of_string (String.prefix goal 3))) + acc) goals in
  helper 0 goal_strs;;
      
let part_a () =
  (Sys.get_argv ()).(1)
  |> In_channel.read_lines
  |> solve ~n:2
  |> printf "Total complexity: %d\n"
;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> In_channel.read_lines
  |> solve ~n:25
  |> printf "Total complexity: %d\n"
;;

part_a ();;
part_b ();;

  
