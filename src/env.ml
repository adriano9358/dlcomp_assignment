

type 'a env = (string * 'a) list list

let empty_env = []

let begin_scope gamma = []::gamma

let end_scope gamma = List.tl gamma

let bind gamma x v = ((x,v)::List.hd gamma)::List.tl gamma

let rec lookup gamma x = 
  match gamma with
  | [] -> None
  | scope::gamma' -> 
    let o = List.assoc_opt x scope in
    if o = None then lookup gamma' x else o