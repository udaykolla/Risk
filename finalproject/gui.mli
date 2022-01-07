(** Graphical user interface.

    This module represents the graphical user interface to play a game. *)

open Graphics
open Camlimages
open State
open Player
open Board

(** The type [click] represents a clickable item on the display. *)
type click =
  | Nothing
  | Region of region_id
  | Plus
  | Minus
  | Enter
  | Place
  | Attack
  | Move
  | Trade
  | End

(** [update brd st] updates the display of the game using [brd] to
    reflect being in state [st]. *)
val update : Board.t -> State.t -> unit

(** [init brd st] initializes the display of the game using [brd] to
    reflect being in state [st]. *)
val init : Board.t -> State.t -> unit

(** [show_current brd st s] displays [s], which corresponds to the
    user's recent clicks. If a current or error was shown before it is
    replaced. *)
val show_current : Board.t -> State.t -> string -> unit

(** [show_error brd st s] displays [s], which is an error message. If a
    current or error was shown before it is replaced. *)
val show_error : Board.t -> State.t -> string -> unit

(** [get_counter ()] is the current value of the counter on the display. *)
val get_counter : unit -> int

(** [get_command brd st] is the next click-specified command for the
    game in [st] using [brd]. *)
val get_command : Board.t -> State.t -> Command.command

(** [next_click brd] is the next item the user clicks. *)
val next_click : Board.t -> click
