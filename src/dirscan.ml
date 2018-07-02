open Unix
open Utils
   
module Devino = struct
  type t = int * int
  let compare = compare
end

module DS = Set.Make(Devino)

let iter fns f =
  let seen = ref DS.empty in
  let rec iter path fn f =
    cleanup (maybe opendir fn) (whenever closedir)
    @@ whenever
    @@ fun d ->
       let rec loop res =
         match try Some(readdir d) with End_of_file -> None with
         | Some u -> loop (u :: res)
         | None -> res
       in
       let entries = loop [] in
       List.iter
         (function
          | "."|".." -> ()
          | u ->
             let fn' = Filename.concat fn u in
             match maybe lstat fn' with
             | None -> ()
             | Some st ->
                let path' = u :: path in
                f path' fn' st;
                if st.st_kind = S_DIR then
                  (
                    let di = (st.st_dev,st.st_ino) in
                    if not (DS.mem di !seen) then
                      (
                        seen := DS.add di !seen;
                        iter path' fn' f
                      )
                  )
         )
         entries
  in
  List.iter (fun fn -> iter [] fn f) fns
