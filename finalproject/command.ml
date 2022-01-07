type object_phrase = string list

type command =
  | Place of object_phrase
  | Trade
  | Attack of object_phrase
  | Move of object_phrase
  | End
  | Show
  | Help
  | Quit

exception Empty

exception Malformed

let remove_spaces str =
  let words =
    List.filter (fun a -> a <> "") (String.split_on_char ' ' str)
  in
  if List.length words = 0 then raise Empty else words

let get_phrase lst cmd =
  let temp = List.filter (fun a -> a <> cmd) lst in
  List.map
    (fun x -> String.concat " " (String.split_on_char '_' x))
    temp

let parse str =
  let words = remove_spaces str in
  let length = List.length words in
  let cmd = List.hd words in
  if cmd = "place" && length > 1 then Place (get_phrase words cmd)
  else if cmd = "move" && length > 1 then Move (get_phrase words cmd)
  else if cmd = "attack" && length > 1 then
    Attack (get_phrase words cmd)
  else if cmd = "end" && length = 1 then End
  else if cmd = "trade" && length = 1 then Trade
  else if cmd = "show" && length = 1 then Show
  else if cmd = "help" && length = 1 then Help
  else if cmd = "quit" && length = 1 then Quit
  else raise Malformed
