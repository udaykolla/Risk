(** Representation of a command.

    This module represents a command which the user can input in order
    to modify the state of a game. *)

(** The type [object_phrase] represents the object phrase that can be
    part of a player command. Each element of the list represents a word
    of the object phrase, where a {i word} is defined as a consecutive
    sequence of non-space characters. Thus, no element of the list
    should contain any leading, internal, or trailing spaces. The list
    is in the same order as the words in the original player command.
    For example:

    - If the player command is ["move A1 A2"], then the object phrase is
      [\["A1"; "A2"\]].

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Place of object_phrase
  | Trade
  | Attack of object_phrase
  | Move of object_phrase
  | End
  | Show
  | Help
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command]. The first word
    (i.e., consecutive sequence of non-space characters) of [str]
    becomes the verb. The rest of the words, if any, become the object
    phrase. Examples:

    - [parse " move A1 A2 "] is [Move \["A1"; "A2"\]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] contains no non-space characters.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is not valid, or if an empty object phrase
    is given to a verb which requires a non-empty one and vice versa. *)
val parse : string -> command
