open Unix
                                                                  
exception Invalid_timestamp of string
   
let ymd_of_string u = Scanf.sscanf u "%04d-%02d-%02d" (fun y m d -> (y,m,d))

let hms_of_string u =
  try
    Scanf.sscanf u "%02d:%02d:%02d" (fun h m s -> (h,m,s))
  with
  | _ -> Scanf.sscanf u "%02d:%02d" (fun h m -> (h,m,0))

let timestamp_of_string ?(hms=(0,0,0)) u =
  match String.split_on_char ' ' u with
  | [date;time] -> ymd_of_string date, hms_of_string time
  | [date] -> ymd_of_string date, hms
  | _ -> raise (Invalid_timestamp u)

let string_of_tm tm =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let time_string_of_tm tm =
  Printf.sprintf "%02d:%02d:%02d"
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let week = [|"Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"|]

let tm_of_timestamp ((year,mon,day),(hour,min,sec)) =
  mktime 
    {
      tm_sec = sec;
      tm_min = min;
      tm_hour = hour;
      tm_year = year - 1900;
      tm_mon = mon - 1;
      tm_mday = day;
      tm_yday = 0;
      tm_wday = 0;
      tm_isdst = false;
    }

let epoch_of_timestamp ts = fst (tm_of_timestamp ts)

let get_timestamp ?(def=0.0) ?hms spec =
  match spec with
  | None -> def
  | Some ts -> timestamp_of_string ?hms ts |> epoch_of_timestamp
