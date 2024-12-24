open Core;;
let mix n m = n lxor m;;
let prune n = n mod 16777216;;

let next secret =
  let secret = secret*64
               |> mix secret
               |> prune in
  let secret = secret/32
               |> mix secret
               |> prune in
  let secret = secret*2048
               |> mix secret
               |> prune in
  secret

let rec iter_secret n initial =
  if n = 0 then initial
  else iter_secret (n-1) (List.map ~f:next initial)
;;

let part_a () =
  (Sys.get_argv ()).(1)
  |> In_channel.read_lines
  |> List.map ~f:Int.of_string
  |> iter_secret 2000
  |> List.fold ~init:0 ~f:(+)
  |> printf "Secrets total: %d\n"
;;

let price secret = secret mod 10;;

let price_delta secret =
  let secret' = next secret in
  (price secret') - (price secret)
;;

module Pattern = struct
  type t = (int * int * int * int)
  [@@deriving eq,hash,compare,sexp];;
end;;

let best_price_all_patterns secrets =
  (* Construct a two-tier hash table mapping patterns to a mapping of
     monkey index to price paid.

     This takes O(max_steps * num_monkeys) to construct...which is
     only ~5 million, so it takes a second or so but manageable.

     Then we just iterate over the outer hash table finding the max
     sum of the inner table to find the best (pattern, price) pair.
  *)
  let prices = Hashtbl.create (module Pattern) in
  let rec helper steps secrets =
    if steps > (2000 - 4)
    then ()
    else
      (List.iteri secrets ~f:(fun monkeyidx secret ->
           let a      = price_delta secret in
           let secret = next secret in
           let b      = price_delta secret in
           let secret = next secret in
           let c      = price_delta secret in
           let secret = next secret in
           let d      = price_delta secret in
           let pat    = (a,b,c,d) in
           let secret = next secret in
           let mprice  = price secret in
           match Hashtbl.find prices pat with
           | None -> (let data = Hashtbl.create (module Int) in   (* Array.create ~len:num_monkeys None in *)
                      Hashtbl.set data ~key:monkeyidx ~data:mprice;
                      Hashtbl.set prices ~key:pat ~data)                
           | Some arr -> match Hashtbl.find arr monkeyidx with
             | Some _ -> ()
             | None   -> Hashtbl.set arr ~key:monkeyidx ~data:mprice);
       helper (steps+1) (List.map ~f:next secrets))
  in
  helper 0 secrets;
  Hashtbl.fold prices ~init:None ~f:(fun ~key ~data acc ->
    let total_price = Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) data in
    match acc with
    | None               -> Some (key,total_price)
    | Some (_,best_price) -> (if total_price > best_price
                              then Some (key,total_price)
                              else acc));;

let part_b () =
  (Sys.get_argv ()).(1)
  |> In_channel.read_lines
  |> List.map ~f:Int.of_string
  |> best_price_all_patterns
  |> function
  | None -> printf "Failure!\n"
  | Some ((a,b,c,d),score) ->
    printf "Pattern (%d,%d,%d,%d) has optimal score %d\n"
      a b c d score
;;

part_a ();;
part_b ();;
  
