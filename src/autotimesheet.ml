open Cmdliner
open Main

let since =
  let doc = "Only consider events that happened since the given \
             timestamp."
  in
  Arg.(value & opt (some string) None & info ["s";"since"] ~docv:"TIMESPEC" ~doc)

let before =
  let doc = "Only consider events that happened before the given timestamp."
  in
  Arg.(value & opt (some string) None & info ["b";"before"] ~docv:"TIMESPEC" ~doc)

let paths =
  let doc = "Paths to scan" in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"PATH" ~doc)

let detailed =
  let doc = "Display events within a slice" in
  Arg.(value & flag & info ["d";"detailed"] ~doc)

let granularity =
  let doc = "Number of slices per day"
  in
  Arg.(value & opt int 48 & info ["g";"granularity"] ~docv:"INTEGER" ~doc)
  
let process_t = Term.(const process $ detailed $ granularity $ since $ before $ paths)

let my_info =
  let doc = "scan directories" in
  let man = [
      `S "TIMESTAMPS";
      `P "Timestamps must be given in YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS] format, \
          and are expressed in the local timezone.";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/berke/autotimesheet"
    ]
  in
  Term.info "autotimesheet" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit (eval (process_t, my_info)))
