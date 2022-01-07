(** Representation of player.

    This module represents a player of the game, including their name,
    regions owned, and cards held. *)
open Graphics

(** The abstract type of values representing a player. *)
type t

(** The type representing the type of the place, which is either Human,
    or an AI with a specified level of difficulty. *)
type player_type =
  | Human
  | AI of int

(** [player_init n r c t] returns a player with name [n], regions [r],
    color [c], and player type [t]. *)
val init_player :
  string -> Board.region_id list -> Graphics.color -> player_type -> t

(** [name p] is the name of player p *)
val name : t -> string

(** [regions p] is a set-like list of regions owned by player p *)
val regions : t -> Board.region_id list

(** [color p] is the color of player [p] *)
val color : t -> Graphics.color

(** [has_region p r] is whether player [p] owns region [r] *)
val has_region : t -> Board.region_id -> bool

(** [cards p] is a set-like list of cards owned by player p *)
val cards : t -> Card.t list

(** [set_cards p c] sets the cards of player [p] to [c] *)
val set_cards : t -> Card.t list -> unit

(** [add_card p c] adds card [c] to player [p]'s list of cards *)
val add_card : t -> Card.t -> unit

(** [set_regions p r] sets the regions of player [p] to [r] *)
val set_regions : t -> Board.region_id list -> unit

(** [remove_region p r] removes region [r] from player [p]'s list of
    regions *)
val remove_region : t -> Board.region_id -> unit

(** [add_region p r] adds region [r] to player [p]'s list of regions *)
val add_region : t -> Board.region_id -> unit

(** [unplaced_troops p] returns the unplaced troops of player [p] *)
val unplaced_troops : t -> int

(** [set_unplaced_troops p n] sets the unplaced troops of player [p] to
    [n] *)
val set_unplaced_troops : t -> int -> unit

(** [player_type p] is the player type of player [p]. *)
val player_type : t -> player_type
