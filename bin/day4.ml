open Core;;

let get arr x y =
  try
    Array.get arr x
    |> (fun s -> Array.get s y)
    |> Option.return
  with _ -> None
    
let read_input f = In_channel.read_lines f
                   |> List.map ~f:String.to_array
                   |> Array.of_list

let check_loc_a acc arr x y =
  let states = List.map [(1,0); (-1,0); (0,1); (0,-1); (1,1); (1,-1); (-1,1); (-1,-1)]
      ~f:(fun (delta_x, delta_y) -> ((x,y), fun x y -> (x + delta_x, y + delta_y))) in
  String.fold "XMAS"
    ~init:states
    ~f:(fun states c ->
        List.fold_left states ~init:[]
          ~f:(fun new_states ((x,y),next) ->
              match get arr x y with
              | None -> new_states
              | Some c' -> (if Char.equal c c'
                            then (next x y, next)::new_states
                            else new_states)))
  |> List.length
  |> (fun n -> acc + n)
              
let count_xmas ~f arr =
  Array.foldi arr ~init:0 ~f:(fun x acc row ->
      Array.foldi row ~init:acc ~f:(fun y acc _ ->
          f acc arr x y))
    
let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> count_xmas ~f:check_loc_a
  |> printf "XMAS appears %d times\n"
;;

let check_loc_b acc arr x y =
  let ul = get arr x y in
  let c  = get arr (x+1) (y+1) in
  let ur = get arr (x+2) y in
  let ll = get arr x (y+2) in
  let lr = get arr (x+2) (y+2) in
  match ul,c,ur,ll,lr with
  | (Some 'M', Some 'A', Some 'M', Some 'S', Some 'S') -> (acc + 1)
  | (Some 'M', Some 'A', Some 'S', Some 'M', Some 'S') -> (acc + 1)
  | (Some 'S', Some 'A', Some 'M', Some 'S', Some 'M') -> (acc + 1)
  | (Some 'S', Some 'A', Some 'S', Some 'M', Some 'M') -> (acc + 1)
  | _ -> acc
;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> count_xmas ~f:check_loc_b
  |> printf "X-MAS appears %d times\n"
;;

part_a ();;
part_b ();;
