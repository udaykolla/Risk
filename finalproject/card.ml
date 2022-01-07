type symbol =
  | Wildcard
  | Infantry
  | Cavalry
  | Artillery

type t = {
  picture : symbol;
  region : Board.region_id;
}

(** [init_card s r] is a card with picture [s] and region [r] *)
let init_card s r = { picture = s; region = r }

(** [picture t] is the picture of card [t] *)
let picture t = t.picture

(** [region t] is the region of card [t] *)
let region t = t.region

let string_of_picture t =
  match t.picture with
  | Wildcard -> "Wildcard"
  | Infantry -> "Infantry"
  | Cavalry -> "Cavalry"
  | Artillery -> "Artillery"

let string_of_card t =
  if t.picture = Wildcard then "Wildcard"
  else t.region ^ " - " ^ string_of_picture t
