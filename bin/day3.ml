open Core;;
type term = Mul of int * int
          | Do
          | Dont;;


let scan_char c abrt cont chan = function
  | Some x -> (if Char.equal x c
               then cont chan (In_channel.input_char chan)
               else abrt (Some x))
  | None -> abrt None

let scan_char_choice abrt xs chan = function
  | Some x ->
    List.find_map ~f:(fun (c,cont) ->
        if Char.equal x c
        then cont chan (In_channel.input_char chan) |> Option.return
        else None) xs
    |> Option.value_or_thunk ~default:(fun _ -> abrt (Some x))
  | None -> abrt None
            
let scan_num abrt cont chan =
  let rec helper acc =
    function
    | Some c -> (if Char.is_digit c
                 then helper (acc*10 + Int.of_string (String.of_char c)) (In_channel.input_char chan)
                 else cont acc chan (Some c))
    | None -> cont acc chan None in
  function
  | Some c -> (if Char.is_digit c
               then helper 0 (Some c)
               else abrt (Some c))
  | None -> abrt None

let scan_ul abrt cont =
  (scan_char 'u' abrt
     (scan_char 'l' abrt
        (scan_char '(' abrt
           (scan_num abrt (fun n ->
                (scan_char ',' abrt
                   (scan_num  abrt (fun m ->
                        scan_char ')' abrt
                          (cont (Mul (n,m)))))))))))
  
let scan_mul abrt cont = scan_char 'm' abrt (scan_ul abrt cont);;

let part_a () =
  let rec scan_all acc chan = function
    | None -> acc
    | Some c ->
      let start_pos = In_channel.pos chan in
      scan_mul (fun c -> (if Int64.equal start_pos (In_channel.pos chan)
                          then In_channel.input_char chan
                          else c)
                         |> scan_all acc chan)
        (function
          | (Mul (x,y)) -> scan_all (acc + (x*y))
          | _ -> scan_all acc)
          chan (Some c)
  in
  (Sys.get_argv ()).(1)
  |> In_channel.with_file ~f:(fun chan -> scan_all 0 chan (In_channel.input_char chan))
  |> printf "Total is %d\n"
;;

let scan_do_dont_mul abrt cont =
  scan_char_choice abrt
    ['m', scan_ul abrt cont;
     'd', scan_char 'o' abrt
       (scan_char_choice abrt
          ['(', scan_char ')' abrt (cont Do);
           'n', (scan_char '\'' abrt
                   (scan_char 't' abrt
                      (scan_char '(' abrt
                         (scan_char ')' abrt (cont Dont)))))]
                        )]

let part_b () =
  let rec scan_all enabled acc chan = function
    | None -> acc
    | Some c ->
      let start_pos = In_channel.pos chan in
      scan_do_dont_mul
        (fun c -> (if Int64.equal start_pos (In_channel.pos chan)
                   then (In_channel.input_char chan)
                   else c)
                  |> scan_all enabled acc chan)
        (function
          | Mul (x,y) -> scan_all enabled (if enabled then (acc + (x*y)) else acc)
          | Do        -> scan_all true acc
          | Dont      -> scan_all false acc)
        chan (Some c)
  in
  (Sys.get_argv ()).(1)
  |> In_channel.with_file ~f:(fun chan -> scan_all true 0 chan (In_channel.input_char chan))
  |> printf "Total is %d\n"
;;

part_a ();;
part_b ();;
