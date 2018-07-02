open Printf
open Unix
               
module Timestamp = struct
  type t = float
  let compare (x : t) y = compare x y
end

module TM = Map.Make(Timestamp)

type work_event =
  File of string list

let prev oc = function
  | File p -> fprintf oc "F %s" (String.concat "/" (List.rev p))

module Day = struct
  type t = int * int * int
  let compare = compare
end

module DM = Map.Make(Day)
                
type work_granule = work_event TM.t

type work_day =
  {
    date     : tm;
    granules : work_granule array;
  }

let prhm oc tod =
  let hour = Pervasives.truncate (tod /. 3600.0) in
  let min = Pervasives.truncate ((mod_float tod 3600.0) /. 60.0) in
  Printf.fprintf oc "%02d:%02d" hour min

let process detailed granularity since before paths =
  let days = ref DM.empty in

  let t_since,t_before = Timeconv.(get_timestamp since, get_timestamp ~def:infinity ~hms:(23,59,59) before) in

  let t_granule = 86400.0 /. float granularity in

  let make_work_day tm = { date = tm; granules = Array.make granularity TM.empty } in
  
  let add t ev =
    if t_since <= t && t <= t_before then
      (
        let tm = localtime t in
        let day = tm.tm_year + 1900,tm.tm_mon + 1,tm.tm_mday in
        let wd =
          match DM.find_opt day !days with
          | Some wd -> wd
          | None ->
             let wd = make_work_day tm in
             days := DM.add day wd !days;
             wd
        in
        let t_day_start,_ = mktime { tm with tm_hour = 0; tm_min = 0; tm_sec = 0 } in
        let g = Pervasives.truncate ((t -. t_day_start) /. t_granule) in
        wd.granules.(g) <- TM.add t ev wd.granules.(g)
      )
  in
  
  Dirscan.iter paths
    (fun path fn { st_atime; st_mtime; st_ctime } ->
      let g t = add t (File path) in
      g st_atime;
      g st_mtime;
      g st_ctime);

  DM.iter (fun (y,m,d) { granules } ->
      let n_evs = TM.cardinal (Array.fold_left (fun tm1 tm2 -> TM.union (fun t ev1 _ -> Some ev1) tm1 tm2) TM.empty granules) in
      if n_evs > 0 then
        (
          let worked = Array.fold_left (fun tot wd -> if TM.is_empty wd then tot else tot + 1) 0 granules in
          let h_worked = float worked *. 24.0 /. float granularity in
          let _,tm = Timeconv.tm_of_timestamp ((y,m,d),(0,0,0)) in
          Printf.printf "%s %04d-%02d-%02d - %.1f h - %d event%s\n"
            Timeconv.week.(tm.tm_wday) y m d h_worked n_evs (if n_evs = 1 then "" else "s");
            for g = 0 to granularity - 1 do
              let wd = granules.(g) in
              let n_evs = TM.cardinal wd in
              if n_evs > 0 then
                (
                  Printf.printf "  %a to %a - %d event%s\n"
                    prhm (float g *. t_granule)
                    prhm (float (g + 1) *. t_granule)
                    n_evs (if n_evs = 1 then "" else "s");
                  if detailed then TM.iter (fun t ev ->
                      let tm = localtime t in
                      Printf.printf "    %s %a\n"
                        (Timeconv.time_string_of_tm tm)
                        prev ev)
                    wd
                )
            done))
    !days
