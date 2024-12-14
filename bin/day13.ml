open Core;;
type machine = {
  a : (int * int);
  b : (int * int);
  prize : (int * int)
};;  

let show_machine {a = (a_x, a_y); b = (b_x,b_y); prize = (p_x,p_y)} =
  sprintf "{a = (%d,%d); b = (%d,%d), p = (%d,%d)}"
    a_x a_y b_x b_y p_x p_y;;
let _ = show_machine;;

type state = Init
           | AwaitB of (int * int)
           | AwaitPrize of ((int * int) * (int * int))
           | Next
let read_input f =
  In_channel.with_file f ~f:(
      In_channel.fold_lines ~init:(Init,[]) ~f:(fun (state,rest) l ->
          match state with
          | Init ->
            Scanf.sscanf l "Button A: X+%d, Y+%d"
              (fun x y -> (AwaitB (x,y),rest))
          | AwaitB a ->
            Scanf.sscanf l "Button B: X+%d, Y+%d"
              (fun x y -> (AwaitPrize (a,(x,y)), rest))
          | AwaitPrize (a,b) ->
            Scanf.sscanf l "Prize: X=%d, Y=%d"
              (fun x y -> (Next, {a;b;prize = (x,y)}::rest))
          | Next -> (Init, rest)
      ))
  |> snd
  |> List.rev

let solve_linalg {a = (a_x,a_y); b = (b_x,b_y); prize = (p_x,p_y)} =
  (*   Two equations in two unknowns:
       a_x * a + b_x * b = p_x
       a_y * a + b_y * b = p_y

       Solve both for a:
       a = (p_x - b_x * b) / a_x
       a = (p_y - b_y * b) / a_y

       Set them equal:
       (p_y - b_y * b) / a_y = (p_x - b_x * b) / a_x

       Solve for b:
       a_x * (p_y - b_y * b) = a_y * ( p_x - b_x * b)
       a_x * p_y - a_x * b_y * b = a_y * p_x - a_y * b_x * b
       a_x * p_y - a_y * p_x = a_x * b_y * b - a_y * b_x * b
       (a_x * p_y - a_y * p_x) = (a_x * b_y - a_y * b_x) * b
       b = (a_x * p_y - a_y * p_x)/(a_x * b_y - a_y * b_x)

  *)
  let b = (a_x * p_y - a_y * p_x)/(a_x * b_y - a_y * b_x) in
  let a = (p_x - (b_x * b)) / a_x in
  if a > 0 && b > 0 && a*a_x + b*b_x = p_x && a*a_y + b*b_y = p_y
  then Some (3*a + b)
  else None
;;

let solve_all =
  List.fold_left ~init:0 ~f:(fun tokens machine ->
      solve_linalg machine      
      |> Option.map ~f:(fun toks ->
          tokens + toks)
      |> Option.value ~default:tokens);;

let scale_machine m = {m with prize = ((fst m.prize) + 10000000000000,
                                       (snd m.prize) + 10000000000000)};;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> solve_all
  |> printf "Tokens required: %d\n"
  |> fun _ -> Out_channel.flush stdout
;;    
let _ = part_a;;

let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> List.map ~f:scale_machine
  |> solve_all
  |> printf "Tokens required: %d\n"
;;    

part_a ()
;;
part_b ()
;;
