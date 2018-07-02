let cleanup x g f =
  try
    let y = f x in
    g x;
    y
  with
  | e -> g x; raise e

let maybe f x =
  try
    Some(f x)
  with
  | e -> None

let whenever f = function
  | None -> ()
  | Some x -> f x

let default x = function
  | None -> x
  | Some y -> y

let default' f x = function
  | None -> f x
  | Some y -> y
