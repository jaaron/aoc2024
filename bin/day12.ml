open Core;;

let read_input f =
    In_channel.read_lines f
    |> List.map ~f:String.to_array
    |> Array.of_list
;;

let get m x y = try m.(x).(y) |> Option.return with _ -> None
let get_exn m x y = m.(x).(y);;
let set m x y v = try m.(x).(y) <- v with _ -> ()

let find_regions m =
    let regmap = Array.init (Array.length m) ~f:(fun _ -> Array.create ~len:(Array.length m.(0)) None) in
    let r_regmap = Hashtbl.create (module Int) in
    let add_to_region reg row col =
        set regmap row col (Some reg);
        Hashtbl.update r_regmap reg
            ~f:(fun locs -> Option.value locs ~default:[]
                            |> fun locs -> (row,col)::locs) in
    let merge_regions reg1 reg2 =
        if reg1 <> reg2
        then
            (let ys = Hashtbl.find_exn r_regmap reg2 in
             List.iter ys ~f:(fun (row,col) -> add_to_region reg1 row col);
             Hashtbl.set r_regmap ~key:reg2 ~data:[])
    in
    Array.iteri m
        ~f:(fun row ->
               Array.iteri ~f:(fun col crop ->
                                  match get m (row-1) col, get m row (col-1) with
                                  | Some crop_a, None ->
                                      if Char.equal crop_a crop
                                      then add_to_region (get_exn regmap (row-1) col |> Option.value_exn) row col
                                      else add_to_region (Hashtbl.length r_regmap) row col
                                  | None, Some crop_l ->
                                      if Char.equal crop_l crop
                                      then add_to_region (get_exn regmap row (col-1) |> Option.value_exn) row col
                                      else add_to_region (Hashtbl.length r_regmap) row col
                                  | None, None -> add_to_region (Hashtbl.length r_regmap) row col
                                  | Some crop_a, Some crop_l ->
                                      if Char.equal crop_a crop && Char.equal crop_l crop
                                      then (merge_regions (get_exn regmap row (col-1) |> Option.value_exn)
                                                (get regmap (row-1) col |> Option.value_exn |> Option.value_exn);
                                            add_to_region (get_exn regmap row (col-1) |> Option.value_exn) row col)
                                      else if Char.equal crop_a crop
                                      then add_to_region (get_exn regmap (row-1) col |> Option.value_exn) row col
                                      else if Char.equal crop_l crop
                                      then add_to_region (get_exn regmap row (col-1) |> Option.value_exn) row col
                                      else add_to_region (Hashtbl.length r_regmap) row col
                              ))
    ;
    r_regmap
;;

let region_area _ = List.length;;

let region_perimeter map region =
    List.fold_left region ~init:0
           ~f:(fun acc (row,col) ->
                  let v = get_exn map row col in
                  List.fold_left ~init:acc
                      ~f:(fun acc (dr,dc) ->
                             match get map (row + dr) (col + dc) with
                             | Some a -> if Char.equal a v then acc else acc + 1
                             | None -> acc + 1)
                      [(-1,0);(1,0);(0,-1);(0,1)])
;;

let count_fences map regions =
    Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data:region acc ->
                                let area = region_area map region in
                                let perimeter = region_perimeter map region in
                                if area > 0
                                then acc + (area*perimeter)
                                else acc)
        regions;;

let part_a () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> fun map ->
    find_regions map
    |> count_fences map
    |> printf "Total fencing is %d\n"
;;

let region_sides map region =
    List.fold_left region ~init:0
        ~f:(fun acc (row,col) ->
               let crop  = get_exn map row col in
               let same_crop x = Option.map ~f:(Char.equal crop) x
                                 |> Option.value ~default:false in
               let north = get map (row-1) col |> same_crop in
               let south = get map (row+1) col |> same_crop in
               let west  = get map row (col-1) |> same_crop in
               let east  = get map row (col+1) |> same_crop in
               let nw    = get map (row-1) (col-1) |> same_crop in
               let ne    = get map (row-1) (col+1) |> same_crop in
               let sw    = get map (row+1) (col-1) |> same_crop in
               let se    = get map (row+1) (col+1) |> same_crop in
               let acc   = (if (not west) && ((not north) || nw)
                            then acc + 1 else acc) in
               let acc   = (if (not north) && ((not east) || ne)
                            then acc + 1 else acc) in
               let acc   = (if (not east) && ((not south) || se)
                            then acc + 1 else acc) in
               let acc   = (if (not south) && ((not west) || sw)
                            then acc + 1 else acc) in
               acc)
        

let count_fences_bulk map regions =
    Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data:region acc ->
                                let area = region_area map region in
                                let sides = region_sides map region in
                                if area > 0
                                then acc + (area*sides)
                                else acc)
        regions;;


let part_b () =
    (Sys.get_argv ()).(1)
    |> read_input
    |> fun map ->
    find_regions map
    |> count_fences_bulk map
    |> printf "Total bulk fencing is %d\n"
;;

part_a ();;
part_b ();;
