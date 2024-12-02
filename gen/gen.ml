let generate_rules base =
  Printf.printf
    {|
(rule
    (target %s.test.output)
    (deps examples/%s.txt)
    (action (with-stdout-to %%{target} (run bin/%s.exe %%{deps}))))
(rule
    (alias runtest)
    (action (diff %s.test.output examples/%s.output)))
    |}
    base base base base base
;;

let () =
  Sys.readdir "bin"
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (fun s -> try
                         Scanf.sscanf s "day%d.ml" (fun d -> Some (Printf.sprintf "day%d" d))
                       with _ -> None)
  |> List.iter generate_rules
;;
    
