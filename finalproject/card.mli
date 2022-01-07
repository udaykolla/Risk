(** Representation of a card.

    This module represents a card, which has a region and a symbol. *)

(** The abstract type of values representing a card. *)
type t

(** The type representing the picture on the card, which could be
    Infantry, Cavalry, Artillery, or the card could be a Wildcard. *)
type symbol =
  | Wildcard
  | Infantry
  | Cavalry
  | Artillery

(** [init_card s r] is a card with picture [s] and region [r] *)
val init_card : symbol -> Board.region_id -> t

(** [picture c] is the picture of card [c] *)
val picture : t -> symbol

(** [region c] is the region of card [c] *)
val region : t -> Board.region_id

(** [string_of_card c] is the string representation of card [c] *)
val string_of_card : t -> string
