(** Representation of dynamic game state.

    This module represents the state of the game, including whose turn
    it is, what phase they are in, the owner of each territory,
    remaining players, the number of troops in each territory, and the
    functions that cause the state to change *)

(** The abstract type of values representing the game state. *)
type t

(** The type representing the current phase during a turn of the game.
    Preturn is an internal phase that allows us to initialize variables
    and prepare for the turn. Place is when the acting player should
    trade and place their placeable troops. Attack is when they should
    attack other opponents regions. AttackMove is an internal phase
    which allows us to query how many troops the acting player wants to
    move to a captured region. Fortify is when the acting player can
    move troops between two neighboring regions that are both owned by
    them. *)
type phase =
  | Preturn
  | Place
  | Attack
  | AttackMove of (Board.region_id * Board.region_id * int)
  | Fortify

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

(** [get_deck st] is the deck of cards in state [st]. *)
val get_deck : t -> Card.t list

(** [get_trades st] is the number of trades so far in the game by state
    [st]. *)
val get_trades : t -> int

(** [init_state_with_args a b c d e f] is a state given all of its
    fields as parameters. This is used in testing to create copies of
    states that don't point to the same place in memory. *)
val init_state_with_args :
  (Board.region_id * int) list ->
  phase ->
  Player.t list ->
  Card.t list ->
  int ->
  bool ->
  t

(** [init_state board num_players] is the initial state of the game when
    playing. The number of players is specified, and the territories are
    randomly assigned to the number of players. Next, random sequence of
    players is generated. Each territory is assigned 2 troops by
    default. All players are humans. *)
val init_state : Board.t -> int -> t

(** [init_state_ai board player_types] is the initial state of the game.
    It is the same as init_state, with the only difference being that
    some/all of the players can now be AIs. This is represented in
    [player_types], where there is an entry for each player, with 0
    representing Human and other positive integers representing an AI of
    varying difficulty. *)
val init_state_ai : Board.t -> int list -> t

(** [turn_order st] refers to the order in which the non-eliminated
    players of the game will take their next turn.

    For example, say the game began with players (P1,P2,P3), P1 has been
    eliminated, and it is currently P2's turn. The order would be (P2,
    P3). After P2 finishes their turn, the order would be (P3, P2). *)
val turn_order : t -> Player.t list

(** [acting_player st] is the player who is currently executing their
    turn. *)
val acting_player : t -> Player.t

(** [acting_phase st] is the current phase of the turn. *)
val acting_phase : t -> phase

(** [placeable_troops st] is the number of troops the current player is
    eligible to place if the acting phase is Place. *)
val placeable_troops : t -> int

(** [territory_captured st] is whether the acting player has captured a
    territory in this turn. *)
val territory_captured : t -> bool

(** [end_phase st b] ends the current phase and proceeds to the next
    one. If a player's turn is over, then go to the next player. If
    current phase is preturn, give troops to current player.

    Requires: [can_end_phase st] is true. *)
val end_phase : t -> Board.t -> unit

(** [shuffle lst] shuffles [lst]. *)
val shuffle : 'a list -> 'a list

(** [can_end_phase st] is whether the phase is allowed to be ended
    according to the rules. A player cannot end Place without placing
    all their troops, or with 5 or more cards in hand. A player must
    move troops into a conquered region before ending Attack.*)
val can_end_phase : t -> bool

(** [string_of_hand st] is the string representation of the acting
    player's hand of cards in [st]. *)
val string_of_hand : t -> string

(** [region_player_troop st] is a list containing the region, its owner,
    and the number of troops, for each region in [st]. *)
val region_player_troop : t -> (Board.region_id * Player.t * int) list

(** [troop_count st reg] gives the number of troops at region [reg] in
    [st]. Requires: [reg] must be a territory that exists in [st]. *)
val troop_count : t -> Board.region_id -> int

(** [reg_tr_map st] is an association list with the regions in [st] as
    keys and and the number of troops in that region in [st] as values. *)
val reg_tr_map : t -> (Board.region_id * int) list

(** [place st reg x] places [x] troops in [reg].

    Requires: [reg] must be a territory that exists and is owned by the
    acting player, who must have at least [x] placeable troops. *)
val place : t -> Board.region_id -> int -> result

(** [move st b move_from move_to num_troops] moves [num_troops] troops
    from one territory to an adjacent territory.

    Requires: [move_from] and [move_to] must be territories that exist
    and are both owned by the acting player. *)
val move :
  t -> Board.t -> Board.region_id -> Board.region_id -> int -> result

(** [attack st b move_from move_to num_dice] simulates an attack of
    [num_dice] dice from [move_from] to [move_to].

    Requires: [move_from] and [move_to] must be territories that exist,
    they must be adjacent to each other, [move_from] must be owned by
    the acting player, [move_from] must have at least two troops, and
    [move_to] must be owned by another player. *)
val attack :
  t -> Board.t -> Board.region_id -> Board.region_id -> int -> result

(** [trade st] automatically makes a trade for the acting player if it
    is possible to do so. *)
val trade : t -> result

(** [owned st reg] is whether the acting player owns region [reg] in
    [st]. *)
val owned_by_current : t -> Board.region_id -> bool
