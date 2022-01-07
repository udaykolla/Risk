(** Dispatcher for user-inputted commands.

    This module is in charge of retrieving commands from the user,
    either through the terminal or GUI. *)

(** [get_command brd st use_gui] is the next inputted command of the
    acting player in [st] of a game using [brd].

    Effects: if acting player is Human, prompts for command through GUI
    if [use_gui], terminal otherwise. *)
val get_command : Board.t -> State.t -> bool -> Command.command
