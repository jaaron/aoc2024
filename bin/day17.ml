open Core;;

module State = struct
  type t = {
    ip : int;
    a  : int;
    b  : int;
    c  : int;
    output : int list
  } [@@deriving eq,compare,hash,sexp];;

  let string_of_state {ip;a;b;c;output} =
    sprintf "{ip = %d; a = %d; b = %d; c = %d; output = \"%s\"}"
      ip a b c (List.rev_map ~f:Int.to_string output
                |> String.concat ~sep:",")
  let _ = string_of_state;;
end;;

type operand_type =
  | Literal
  | Combo

let value_of_operand state n = function
  | Literal -> n
  | Combo   -> (let open State in
                if n <= 3
                then n
                else if n = 4 then state.a
                else if n = 5 then state.b
                else if n = 6 then state.c
                else failwith "Combo operand 7 is reserved")
  
type instr =
  | Adv
  | Bxl
  | Bst
  | Jnz
  | Bxc
  | Out
  | Bdv
  | Cdv

let string_of_instr = function
  | Adv -> "adv"
  | Bxl -> "bxl"
  | Bst -> "bst"
  | Jnz -> "jnz"
  | Bxc -> "bxc"
  | Out -> "out"
  | Bdv -> "bdv"
  | Cdv -> "cdv"

let decode_instr = function
  | 0 -> Adv
  | 1 -> Bxl
  | 2 -> Bst
  | 3 -> Jnz
  | 4 -> Bxc
  | 5 -> Out
  | 6 -> Bdv
  | 7 -> Cdv
  | x -> failwith (sprintf "Invalid opcode %d" x)
;;

let get_instr mem i =
  try Some (decode_instr (mem.(i)), mem.(i+1)) with _ -> None;;
let _ = string_of_instr;;

let step state mem =
  let open State in
  match get_instr mem state.ip with
  | None -> `Done {state with output = List.rev state.output}
  | Some (instr,op) ->
    `State (match instr with
        | Adv -> {state with a = state.a / (1 lsl (value_of_operand state op Combo));
                             ip = state.ip + 2}
        | Bxl -> {state with b = state.b lxor (value_of_operand state op Literal);
                             ip = state.ip + 2}
        | Bst -> {state with b = (value_of_operand state op Combo) mod 8;
                             ip = state.ip + 2}
        | Jnz -> {state with ip = (if state.a = 0
                                   then state.ip + 2
                                   else value_of_operand state op Literal)}
        | Bxc -> {state with b = state.b lxor state.c;
                             ip = state.ip + 2}
        | Out -> let output = ((value_of_operand state op Combo) mod 8)::state.output in
          {state with output; ip = state.ip + 2}
        | Bdv -> {state with b = state.a / (1 lsl (value_of_operand state op Combo));
                             ip = state.ip + 2}
        | Cdv -> {state with c = state.a / (1 lsl (value_of_operand state op Combo));
                             ip = state.ip + 2})
;;

let rec eval state mem =
  match step state mem with
  | `Done state -> state
  | `State state -> eval state mem
;;

let read_input f =
  let open State in
  In_channel.with_file f ~f:(fun chan ->
      let a = In_channel.input_line chan
              |> Option.value_exn
              |> fun s -> Scanf.sscanf s "Register A: %d" (fun n -> n) in
      let b = In_channel.input_line chan
              |> Option.value_exn
              |> fun s -> Scanf.sscanf s "Register B: %d" (fun n -> n) in
      let c = In_channel.input_line chan
              |> Option.value_exn
              |> fun s -> Scanf.sscanf s "Register C: %d" (fun n -> n) in
      let _ = In_channel.input_line chan in
      let prog = In_channel.input_line chan
                 |> Option.value_exn
                 |> String.lsplit2_exn ~on:':'
                 |> snd
                 |> String.strip
                 |> String.split ~on:','
                 |> List.map ~f:Int.of_string
                 |> Array.of_list in
      ({ip = 0; a; b; c; output = []}, prog))
     ;;

let () =
  let open State in
  let state = {ip = 0; a = 0; b = 0; c = 9; output = []} in
  let () = assert ((eval state [|2;6|]).b = 1) in
  let state = {ip = 0; a = 10; b = 0; c = 0; output = []} in
  let () = assert (List.equal (=) (eval state [|5;0;5;1;5;4|]).output [0;1;2]) in
  let state = {ip = 0; a = 2024; b = 0; c = 0; output = []} in
  let state = eval state [|0;1;5;4;3;0|] in
  let () = assert (List.equal (=) state.output [4;2;5;6;7;7;7;7;3;1;0]) in
  let () = assert (state.a = 0) in
  let state = {ip = 0; a = 0; b = 29; c = 0; output = []} in
  let state = eval state [|1;7|] in
  let () = assert (state.b = 26) in
  let state = {ip = 0; a = 0; b = 2024; c = 43690; output = []} in
  let state = eval state [|4;0|] in
  let () = assert (state.b = 44354) in
  ();;

let part_a () =
  (Sys.get_argv ()).(1)
  |> read_input
  |> (fun (state,prog) ->
      let () = printf "mem: [|%s|]\n" (Array.to_list prog |> List.map ~f:Int.to_string |> String.concat ~sep:";") in
      eval state prog)
  |> (fun state -> state.output)
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:","
  |> printf "Output: %s\n"
  |> fun _ -> Out_channel.flush stdout
;;

part_a ();;

(*
   This is all special cased for the actual provided program and
   initial state.

   `exec` is a direct implementation of the program (we could reuse
   eval above if we really wanted to).

   `find_quine` works backwards through the desired output to derive a
   3-bits at a time.

   Note that backtracking is required since the value output can
   depend on up to 10 low order bits of a; changing only the low order
   3-bits may not achieve the desired output. 
*)
let exec a =
  let b = a mod 8 in
  let b = b lxor 3 in
  let c = a lsr b in
  let b = b lxor c in
  let b = b lxor 5 in
  (b mod 8)
  ;;

let find_quine prog =
  let rec helper k a n = function
    | [] -> Some a
    | x::xs -> (if n = 8 then k ()
                else if exec ((a lsl 3) lor n) = x
                then helper (fun () -> helper k a (n+1) (x::xs)) ((a lsl 3) lor n) 0 xs
                else helper k a (n+1) (x::xs))
  in
  helper (fun () -> None) 0 0 (List.rev prog)
;;

let part_b () =
  let _state,prog = (Sys.get_argv ()).(1)
                   |> read_input in
  find_quine (Array.to_list prog)
  |> Option.value_exn
  |> printf "Answer is %d\n"
;;

part_b ()
               
