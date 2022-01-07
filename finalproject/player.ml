type player_type =
  | Human
  | AI of int

type t = {
  name : string;
  mutable regions : Board.region_id list;
  mutable cards : Card.t list;
  mutable unplaced_troops : int;
  color : Graphics.color;
  ptype : player_type;
}

let init_player n r c t =
  {
    name = n;
    regions = r;
    cards = [];
    unplaced_troops = 0;
    color = c;
    ptype = t;
  }

let name p = p.name

let regions p = p.regions

let color p = p.color

let has_region p r = List.mem r p.regions

let cards p = p.cards

let set_cards p c = p.cards <- c

let add_card p c = p.cards <- c :: p.cards

let set_regions p r = p.regions <- r

let remove_region p r =
  p.regions <- List.filter (fun x -> x <> r) p.regions

let add_region p r = p.regions <- r :: p.regions

let unplaced_troops p = p.unplaced_troops

let set_unplaced_troops p n = p.unplaced_troops <- n

let player_type p = p.ptype
