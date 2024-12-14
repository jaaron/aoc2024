open Core;;

module IVec2 = struct
  type t = {x : int; y : int} [@@deriving eq];;
  let (+) a b = {x = a.x + b.x; y = a.y + b.y}
  let ( * ) n v = {x = n*v.x; y = n*v.y}
  let ( mod ) a b =
    let open Int in
    let x = a.x mod b.x in
    let x = if x < 0 then b.x + x else x in
    let y = a.y mod b.y in
    let y = if y < 0 then b.y + y else y in
    {x;y}
end;;

type robot = {
  pos : IVec2.t;
  vel : IVec2.t
} [@@deriving eq];;

let read_input f =
  In_channel.read_lines f
  |> List.map ~f:(fun l ->
      Scanf.sscanf l "p=%d,%d v=%d,%d"
        (fun px py vx vy ->
           let open IVec2 in
           {pos = {x = px; y = py}; vel = {x = vx; y = vy}}))
;;

let max_x = 101;;
let max_y = 103;;

let move ~seconds r =
  let open IVec2 in
  {r with pos = (r.pos + (seconds * r.vel)) mod {x = max_x; y = max_y}};;

let move_all ~seconds = List.map ~f:(move ~seconds);;

let safety_factor rs =
  List.fold_left rs ~init:(0,0,0,0)
    ~f:(fun (ul,ur,ll,lr) {pos;_}->
        let is_upper = pos.y < (max_y / 2) in
        let is_lower = pos.y > (max_y / 2) in
        let is_left  = pos.x < (max_x / 2) in
        let is_right = pos.x > (max_x / 2) in
        if is_upper 
        then if is_left
          then (ul+1,ur,ll,lr)
          else if is_right
          then (ul,ur+1,ll,lr)
          else (ul,ur,ll,lr)
        else if is_lower
        then if is_left
          then (ul,ur,ll+1,lr)
          else if is_right
          then (ul,ur,ll,lr+1)
          else (ul,ur,ll,lr)
        else (ul,ur,ll,lr)
      )
  |> fun (ul,ur,ll,lr) ->
  (ul*ur*ll*lr);;

let make_floor rs =
  let floor = Array.init max_y ~f:(fun _ -> Array.create ~len:max_x 0) in
  let () = List.iter rs ~f:(fun {pos;_} -> floor.(pos.y).(pos.x) <- floor.(pos.y).(pos.x) + 1) in
  floor;;

let print_floor rs =
  make_floor rs
  |> Array.iter 
    ~f:(fun r ->
        Array.iter r ~f:(fun x -> printf "%s" (if x = 0 then "." else if x < 10 then Int.to_string x else "+"));
        Out_channel.newline stdout);
  Out_channel.flush stdout
;;

let _ = print_floor;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> move_all ~seconds:100
  |> safety_factor
  |> printf "Safety factor is %d\n"
;;

part_a ();;

let check_treeish rs =
  let floor = make_floor rs in
  let is_occupied IVec2.{x;y} = try floor.(x).(y) > 0 with _ -> false in
  List.exists rs ~f:(fun {pos;_} ->
    let rec helper i = if i > 5 then true
        else (is_occupied IVec2.(pos + {x = (0-i); y = (0-i)}) &&
              is_occupied IVec2.(pos + {x = i; y = (0-i)}) &&
              helper (i+1)) in
    helper 1)
;;
             
             
let find_tree rs =
  let rec helper n rs =
    if n > 65536 then None
    else
        if check_treeish rs
        then Some n
        else helper (n+1) (move_all ~seconds:1 rs)
  in helper 0 rs;;

  
let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> find_tree
  |> Option.map ~f:(printf "Christmas tree after %d seconds\n")
  |> Option.value_or_thunk ~default:(fun _ -> printf "Unable to find christmas tree\n")
;;
let _ = part_b ;;
part_b ();;
