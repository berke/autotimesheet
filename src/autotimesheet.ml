open Cmdliner
   
let process path =
  Printf.printf "Processing %s\n%!" path

let path =
  let doc = "Path to scan" in
  Arg.(value & opt string "." & info ["p";"path"] ~docv:"PATH" ~doc)
  
let process_t = Term.(const process $ path)

let my_info =
  let doc = "scan directories" in
  let man = [
      `S Manpage.s_bugs;
      `P "Send bugs to foo@bar.com"
    ]
  in
  Term.info "autotimesheet" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit (eval (process_t, my_info)))
