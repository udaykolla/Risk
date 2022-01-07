(** Representation of static board data.

    This module represents the data stored in board files, including the
    regions and continents. It handles loading of that data from JSON as
    well as querying the data. *)

(** The abstract type of values representing boards. *)
type t

(** The type of region identifiers. *)
type region_id = string

(** The type of continent identifiers. *)
type continent_id = string

(** Raised when an unknown region is encountered. *)
exception UnknownRegion of region_id

(** Raised when an unknown continent is encountered. *)
exception UnknownContinent of continent_id

(** [from_json j] is the board that [j] represents. Requires: [j] is a
    valid JSON board representation. *)
val from_json : Yojson.Basic.t -> t

(** [region_ids b] is a set-like list of all of the region identifiers
    in board [b]. *)
val region_ids : t -> region_id list

(** [coordinates b] is a set-like list of all of the region identifiers
    with their coordinates in board [b] *)
val coordinates : t -> (region_id * (int * int)) list

(** [num_regions b] is the number of regions in board [b] *)
val num_regions : t -> int

(** [is_region b r] is whether [b] contains a region with identifier
    [r]. *)
val is_region : t -> region_id -> bool

(** [region_description b r] is the description of region [r] in board
    [b]. Raises [UnknownRegion r] if [r] is not a region identifier in
    [b]. *)
val region_description : t -> region_id -> string

(** [neighbors b r] is a set-like list of all regions adjacent to [r] on
    board [b]. Raises [UnknownRegion r] if [r] is not a region
    identifier in [b]. *)
val neighbors : t -> region_id -> region_id list

(** [continent_ids b] is a set-like list of all of the continent
    identifiers in board [b]. *)
val continent_ids : t -> continent_id list

(** [continent_description b c] is the description of continent [c] in
    board [b]. Raises [UnknownContinent c] if [c] is not a continent
    identifier in [b]. *)
val continent_description : t -> continent_id -> string

(** [continent_bonus b c] is the bonus for continent [c] in board [b].
    Raises [UnknownContinent c] if [c] is not a continent identifier in
    [b]. *)
val continent_bonus : t -> continent_id -> int

(** [regions_in_continent b c] is a set-like list of all regions within
    a continent with identifier [c] in board [b]. Raises
    [UnknownContinent c] if [c] is not a continent identifier in [b]. *)
val regions_of_continent : t -> continent_id -> region_id list

(** [continent_of_region b r] is the identifier of the continent that
    contains region [r] in board [b]. Raises [UnknownRegion r] if [r] is
    not a region identifier in [b]. *)
val continent_of_region : t -> region_id -> continent_id

(** [is_neighbor b r1 r2] checks if region [r2] is a neighbor of region
    [r1] in [b]. Raises [UnknownRegion r1] if [r1] is not a region
    identifier in [b]. *)
val is_neighbor : t -> region_id -> region_id -> bool

(** [min_players b] is the minimum number of players needed to play a
    game using [b]. *)
val min_players : t -> int

(** [max_players b] is the maximum number of players needed to play a
    game using [b]. *)
val max_players : t -> int

(** [troops_per_player b] is the list of potential values for numbers of
    troops per player depending on number of players, with the first
    entry representing the value for 2 players on board [b] *)
val troops_per_player : t -> int list
