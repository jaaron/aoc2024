open Core;;

type wire = String.t;;
type gate = And of (wire * wire * wire)
          | Or of (wire * wire * wire)
          | Xor of (wire * wire * wire)

let rec get_output gates wires =
  let get_wire w = Hashtbl.find wires w
                   |> Option.value_or_thunk ~default:(fun () ->
                       Hashtbl.find_exn gates w
                       |> get_output gates wires)
  in
  function
  | And (in1,in2,out) -> 
    let in1v = get_wire in1 in
    let in2v = get_wire in2 in
    let res  = in1v && in2v in
    let ()   = Hashtbl.set wires ~key:out ~data:res in
    res
  | Or (in1,in2,out) -> 
    let in1v = get_wire in1 in
    let in2v = get_wire in2 in
    let res  = in1v || in2v in
    let ()   = Hashtbl.set wires ~key:out ~data:res in
    res
  | Xor (in1,in2,out) -> 
    let in1v = get_wire in1 in
    let in2v = get_wire in2 in
    let res  = not (Bool.equal in1v in2v) in
    let ()   = Hashtbl.set wires ~key:out ~data:res in
    res
;;
      
let read_input f =
  let wires = Hashtbl.create (module String) in
  let gates = Hashtbl.create (module String) in
  In_channel.with_file f
    ~f:(fun chan ->
        In_channel.fold_lines chan ~init:`ReadWires ~f:(fun state line ->
            match state with
            | `ReadWires -> (match String.strip line with
                | "" -> `ReadGates
                | line ->
                  Scanf.sscanf line "%c%c%c: %d" (fun a b c state ->
                      Hashtbl.set wires ~key:(String.of_list [a;b;c]) ~data:(state = 1);
                      `ReadWires))
            | `ReadGates ->
              Scanf.sscanf line "%s %s %s -> %s"
                (fun in1 op in2 out ->
                   let g = match op with
                     | "AND" -> And (in1, in2, out)
                     | "OR"  -> Or (in1, in2, out)
                     | "XOR" -> Xor (in1, in2, out)
                     | _ -> failwith "Invalid gate operation"
                   in
                   Hashtbl.set gates ~key:out ~data:g;
                   `ReadGates)))
  |> ignore;
  (gates, wires)

let get_circuit_output (gates, wires) =
  let all_wires = (Hashtbl.keys gates)@(Hashtbl.keys wires)
                  |> List.dedup_and_sort ~compare:String.compare in
  let zs = all_wires
           |> List.filter ~f:(fun w ->
               Char.equal (String.get w 0) 'z' &&
               Char.is_digit (String.get w 1) &&
               Char.is_digit (String.get w 2))
           |> List.sort ~compare:String.compare in
  List.fold_left zs ~init:0 ~f:(fun acc z ->
      let bit  = get_output gates wires (Hashtbl.find_exn gates z) in
      if bit
      then 
        let bpos = Int.of_string (String.suffix z 2) in
        acc + (1 lsl bpos)
      else acc)
;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> get_circuit_output
  |> printf "Circuit output is %d\n"
;;

                
         
part_a ();;

let make_dot (gates, wires) outf =
  Out_channel.with_file outf ~f:(fun chan ->
      let () = fprintf chan "digraph {\n" in
      let () = Hashtbl.keys wires |> List.iter ~f:(fun w ->
          let c0 = String.get w 0 in
          if (Char.equal c0 'x') || (Char.equal c0 'y') then
            (fprintf chan {dot|"%s" [style=filled,fillcolor=%s];|dot} w
               (if (Char.equal c0 'x') then "orange" else "purple");
             Out_channel.newline chan)) in
      let () = Hashtbl.iter gates ~f:(function
          | And (in1, in2, out) ->
            let nid = sprintf "%s AND %s -> %s" in1 in2 out in
            fprintf chan {dot|"%s" [label = "%s",style=filled,fillcolor=yellow];|dot} nid "AND";
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} in1 nid;
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} in2 nid;
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} nid out;
            Out_channel.newline chan;
          | Or (in1, in2, out) ->
            let nid = sprintf "%s OR %s -> %s" in1 in2 out in
            fprintf chan {dot|"%s" [label = "%s",style=filled,fillcolor=green];|dot} nid "OR";
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} in1 nid;
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} in2 nid;
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} nid out;
            Out_channel.newline chan;
          | Xor (in1, in2, out) ->
            let nid = sprintf "%s XOR %s -> %s" in1 in2 out in
            fprintf chan {dot|"%s" [label = "%s",style=filled,fillcolor=blue];|dot} nid "XOR";
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} in1 nid;
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} in2 nid;
            Out_channel.newline chan;
            fprintf chan {dot|"%s" -> "%s";|dot} nid out;
            Out_channel.newline chan;
        ) in
      fprintf chan "}"
    );;
     

let is_input w = let c = String.get w 0 in
  (Char.equal c 'x') || (Char.equal c 'y')
                        
let rec check_output_bit gates w =
  match Hashtbl.find_exn gates w with
  | Or (in1, in2, _) ->
    if String.equal w "z45"
    then check_carry_bits gates in1 in2
    else Error (sprintf "Bad output bit %s" w)
  | Xor (in1, in2, _) -> check_xor1_ins gates in1 in2
  | _ -> Error (sprintf "Bad output bit %s" w)
and check_carry_bits gates in1 in2 =
  match (Hashtbl.find_exn gates in1,
         Hashtbl.find_exn gates in2) with
  | (And (in11, in12,_),
     And (in21, in22,_)) ->
    if ( ((is_input in11) && (is_input in12)) ||
         ((is_input in21) && (is_input in22)))
    then Ok ()
    else Error (sprintf "Bad carry bits %s and %s" in1 in2)
  | _ -> Error (sprintf "bad carry bits %s and %s"  in1 in2)
and check_xor1_ins gates in1 in2 =
  match Hashtbl.find_exn gates in1,
        Hashtbl.find_exn gates in2 with
  | (
    (Xor (in11, in12,_), Or (in21,in22,_))
  |
    (Or (in21, in22,_), Xor (in11, in12,_))
  ) ->
    if ((is_input in11) && (is_input in12))
    then check_carry_bits gates in21 in22
    else Error (sprintf "Bad xor0 inputs %s %s" in11 in12)
  | _ -> Error (sprintf "Bad xor1 inputs %s %s" in1 in2)

let check_output_bits gates =
  let rec helper acc  n =
    if n = 1 then acc
    else 
      helper (match check_output_bit gates (sprintf "z%02d" n) with
          | Ok () -> acc
          | Error x -> x::acc) (n-1)
  in helper [] 45
;;

(* This isn't actually going to solve the problem, but it will
   identify gates that are faulty and output a dot file that can be
   visualized with graphviz. Visually scanning the offending gates
   pretty easily identifies which outputs are actually swapped.
*)
let part_b () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> fun (gates,wires) ->
  make_dot (gates,wires) "day24.dot";
  check_output_bits gates
  |> List.iter ~f:(printf "%s\n")
;;

part_b ();;

