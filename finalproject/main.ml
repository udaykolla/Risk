let using_gui = ref false

(** [print_string_yellow str] prints [str] in yellow. *)
let print_string_yellow =
  ANSITerminal.print_string [ ANSITerminal.yellow ]

let print_prompt str =
  print_endline str;
  print_string "> "

let print_prompt_command () =
  if not !using_gui then print_prompt "Input next command."

let print_wrong_arg arg msg =
  ANSITerminal.print_string [ ANSITerminal.red ] "Error: ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] arg;
  ANSITerminal.print_string [ ANSITerminal.red ] (" " ^ msg ^ "\n\n")

let print_error str =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("Error: " ^ str ^ "\n\n")

(** [print_strings lst] prints the strings in [lst] in order separated
    by newlines. *)
let rec print_strings lst = ignore (List.map print_endline lst)

(** Flush standard output, then read characters from standard input
    until a newline character is encountered. Return the string of all
    characters read, without the newline character at the end. Then
    print a newline. *)
let read_line_and_print () =
  let x = read_line () in
  print_endline "";
  x

let rec string_of_trp_map map =
  match map with
  | [] -> ""
  | (region, player, troops) :: t ->
      region ^ ": " ^ string_of_int troops ^ " troops: "
      ^ Player.name player ^ "\n" ^ string_of_trp_map t

(** [take_command brd st] prompts for and processes a player-inputted
    command and continues to do so until the game is over or quitted. *)
let rec take_command brd st =
  try
    if !st |> State.turn_order |> List.length = 1 then ()
    else (
      (match State.acting_phase !st with
      | Preturn ->
          let act_plyr_name = Player.name (State.acting_player !st) in
          print_endline ("Starting " ^ act_plyr_name ^ "'s turn!");
          State.end_phase !st brd
      | Place -> process_command_place brd st
      | Attack -> process_command_attack brd st
      | AttackMove (reg_from, reg_to, min) ->
          process_command_attackmove brd st
      | Fortify -> process_command_fortify brd st);
      if !using_gui then Gui.update brd !st;
      take_command brd st)
  with Graphics.Graphic_failure _ ->
    print_endline "";
    process_quit brd st

(** [process_command_place brd st] prompts for and processes a
    player-inputted command for phase Place and continues to prompt for
    and process commands until the game is over or quitted. Requires:
    [st] is in phase Place. *)
and process_command_place brd st =
  print_endline "Phase: Place";
  let to_place = string_of_int (State.placeable_troops !st) in
  print_endline ("You have " ^ to_place ^ " troops to place.");
  print_endline ("Your hand: " ^ State.string_of_hand !st);
  print_prompt_command ();

  match Dispatcher.get_command brd !st !using_gui with
  | Place obj_phr -> process_place obj_phr brd st
  | Trade -> process_trade brd st
  | End -> process_end brd st
  | Attack _ | Move _ -> print_error "cannot use that command now"
  | Show -> process_show brd st
  | Help -> process_help brd st
  | Quit -> process_quit brd st
  | (exception Command.Empty) | (exception Command.Malformed) ->
      process_bad_command brd st

(** [process_command_attack brd st] prompts for and processes a
    player-inputted command for phase Attack and continues to prompt for
    and process commands until the game is over or quitted. Requires:
    [st] is in phase Attack. *)
and process_command_attack brd st =
  print_endline "Phase: Attack";
  print_prompt_command ();

  match Dispatcher.get_command brd !st !using_gui with
  | Attack obj_phr -> process_attack obj_phr brd st
  | End -> process_end brd st
  | Place _ | Trade | Move _ ->
      print_error "cannot use that command now"
  | Show -> process_show brd st
  | Help -> process_help brd st
  | Quit -> process_quit brd st
  | (exception Command.Empty) | (exception Command.Malformed) ->
      process_bad_command brd st

(** [process_command_attackmove brd st] prompts for and processes a
    player-inputted command for phase AttackMove and continues to prompt
    for and process commands until the game is over or quitted.
    Requires: [st] is in phase AttackMove. *)
and process_command_attackmove brd st =
  print_endline "Phase: Attack (Must move)";
  print_prompt_command ();

  match Dispatcher.get_command brd !st !using_gui with
  | Move obj_phr -> process_move_attack obj_phr brd st
  | End -> process_end brd st
  | Attack _ | Place _ | Trade ->
      print_error "cannot use that command now"
  | Show -> process_show brd st
  | Help -> process_help brd st
  | Quit -> process_quit brd st
  | (exception Command.Empty) | (exception Command.Malformed) ->
      process_bad_command brd st

(** [process_command_fortify brd st] prompts for and processes a
    player-inputted command for phase Fortify and continues to prompt
    for and process commands until the game is over or quitted.
    Requires: [st] is in phase Fortify. *)
and process_command_fortify brd st =
  print_endline "Phase: Fortify";
  print_prompt_command ();

  match Dispatcher.get_command brd !st !using_gui with
  | Move obj_phr -> process_move obj_phr brd st
  | End -> process_end brd st
  | Attack _ | Place _ | Trade ->
      print_error "cannot use that command now"
  | Show -> process_show brd st
  | Help -> process_help brd st
  | Quit -> process_quit brd st
  | (exception Command.Empty) | (exception Command.Malformed) ->
      process_bad_command brd st

(** [check_place_1 region brd st] is whether [region] is valid for the
    "place" command.

    Effects: prints an error message if [region] is invalid. *)
and check_place_1 region brd st =
  if Board.is_region brd region then
    if State.owned_by_current !st region then true
    else (
      print_wrong_arg region "is not your territory";
      false)
  else (
    print_wrong_arg region "is not a valid region";
    false)

(** [process_place_full region num brd st] processes when the user has
    specified [region] for the "place" command, while the game is in
    state [st]. *)
and process_place_1 region brd st =
  if check_place_1 region brd st then (
    print_prompt "How many troops do you want to place?";
    let num = read_line_and_print () in
    process_place_full region num brd st)
  else ()

(** [process_place_full region num brd st] processes when the user has
    specified valid [region], and [num] for the "place" command, while
    the game is in state [st]. *)
and process_place_full region num brd st =
  match int_of_string_opt num with
  | Some n -> (
      let result = State.place !st region n in
      match result with
      | Legal new_st -> st := new_st
      | Illegal -> print_wrong_arg num "troops cannot be placed")
  | None -> print_wrong_arg num "is not an integer"

(** [process_place obj_phr brd st] processes when the user uses the
    "place" command with non-empty object phrase [obj_phr]. *)
and process_place obj_phr brd st =
  match obj_phr with
  | [ region ] -> process_place_1 region brd st
  | [ region; num ] ->
      if check_place_1 region brd st then
        process_place_full region num brd st
      else ()
  | _ -> print_error "place accepts 1-2 arguments"

(** [process_trade brd st] processes when the user uses the "trade"
    command with empty object phrase. *)
and process_trade brd st =
  match State.trade !st with
  | Legal new_st -> st := new_st
  | Illegal -> print_error "you do not have a tradable set of cards"

(** [check_attack_1 region1 brd st] is whether [region1] is valid for
    the "attack" command.

    Effects: prints an error message if [region1] is invalid. *)
and check_attack_1 region1 brd st =
  if Board.is_region brd region1 then
    if State.owned_by_current !st region1 then true
    else (
      print_wrong_arg region1 "is not your territory";
      false)
  else (
    print_wrong_arg region1 "is not a valid region";
    false)

(** [process_attack_1 region1 brd st] processes when the user has
    specified [region1] for the "attack" command, while the game is in
    state [st]. *)
and process_attack_1 region1 brd st =
  if check_attack_1 region1 brd st then (
    print_prompt "Which region do you want to attack?";
    let region2 = read_line_and_print () in
    process_attack_2 region1 region2 brd st)
  else ()

(** [check_attack_2 region1 region2 brd st] is whether [region2] is
    valid for the "attack" command with given [region1].

    Effects: prints an error message if [region2] is invalid. *)
and check_attack_2 region1 region2 brd st =
  if Board.is_neighbor brd region1 region2 then
    if State.owned_by_current !st region2 then (
      print_wrong_arg region2
        "is your territory so you cannot attack it";
      false)
    else true
  else (
    print_wrong_arg region2 ("does not neighbor " ^ region1);
    false)

(** [process_attack_2 region1 region2 brd st] processes when the user
    has specified valid [region1], and [region2] for the "attack"
    command, while the game is in state [st]. *)
and process_attack_2 region1 region2 brd st =
  if check_attack_2 region1 region2 brd st then (
    print_prompt "How many dice do you want to attack with?";
    let num = read_line_and_print () in
    process_attack_full region1 region2 num brd st)
  else ()

(** [process_attack_full region1 region2 num brd st] processes when the
    user has specified valid [region1] and [region2], and [num] for the
    "attack" command, while the game is in state [st]. *)
and process_attack_full region1 region2 num brd st =
  match int_of_string_opt num with
  | Some n -> (
      let result = State.attack !st brd region1 region2 n in
      match result with
      | Legal new_st -> st := new_st
      | Illegal -> print_wrong_arg num "dice cannot be used to attack")
  | None -> print_wrong_arg num "is not an integer"

(** [process_attack obj_phr brd st] processes when the user uses the
    "attack" command with non-empty object phrase [obj_phr]. *)
and process_attack obj_phr brd st =
  match obj_phr with
  | [ region1 ] -> process_attack_1 region1 brd st
  | [ region1; region2 ] ->
      if check_attack_1 region1 brd st then
        process_attack_2 region1 region2 brd st
      else ()
  | [ region1; region2; num ] ->
      if
        check_attack_1 region1 brd st
        && check_attack_2 region1 region2 brd st
      then process_attack_full region1 region2 num brd st
      else ()
  | _ -> print_error "attack accepts 1-3 arguments"

(** [check_move_1 region1 brd st] is whether [region1] is valid for the
    "move" command.

    Effects: prints an error message if [region1] is invalid. *)
and check_move_1 region1 brd st =
  if Board.is_region brd region1 then
    if State.owned_by_current !st region1 then true
    else (
      print_wrong_arg region1 "is not your territory";
      false)
  else (
    print_wrong_arg region1 "is not a valid region";
    false)

(** [process_move_1 region1 brd st] processes when the user has
    specified [region1] for the "move" command, while the game is in
    state [st]. *)
and process_move_1 region1 brd st =
  if check_move_1 region1 brd st then (
    print_prompt "Which region do you want to move troops to?";
    let region2 = read_line_and_print () in
    process_move_2 region1 region2 brd st)
  else ()

(** [check_move_2 region1 region2 brd st] is whether [region2] is valid
    for the "move" command with given [region1].

    Effects: prints an error message if [region2] is invalid. *)
and check_move_2 region1 region2 brd st =
  if Board.is_neighbor brd region1 region2 then
    if State.owned_by_current !st region2 then true
    else (
      print_wrong_arg region2 "is not your territory";
      false)
  else (
    print_wrong_arg region2 ("does not neighbor " ^ region1);
    false)

(** [process_move_2 region1 region2 brd st] processes when the user has
    specified valid [region1], and [region2] for the "move" command,
    while the game is in state [st]. *)
and process_move_2 region1 region2 brd st =
  if check_move_2 region1 region2 brd st then (
    print_prompt "How many troops do you want to move?";
    let num = read_line_and_print () in
    process_move_full region1 region2 num brd st)
  else ()

(** [process_move_full region1 region2 num brd st] processes when the
    user has specified valid [region1] and [region2], and [num] for the
    "move" command, while the game is in state [st]. *)
and process_move_full region1 region2 num brd st =
  match int_of_string_opt num with
  | Some n -> (
      let result = State.move !st brd region1 region2 n in
      match result with
      | Legal new_st -> st := new_st
      | Illegal -> print_wrong_arg num "troops cannot be moved")
  | None -> print_wrong_arg num "is not an integer"

(** [process_move obj_phr brd st] processes when the user uses the
    "move" command with non-empty object phrase [obj_phr]. *)
and process_move obj_phr brd st =
  match obj_phr with
  | [ region1 ] -> process_move_1 region1 brd st
  | [ region1; region2 ] ->
      if check_move_1 region1 brd st then
        process_move_2 region1 region2 brd st
      else ()
  | [ region1; region2; num ] ->
      if
        check_move_1 region1 brd st
        && check_move_2 region1 region2 brd st
      then process_move_full region1 region2 num brd st
      else ()
  | _ -> print_error "move accepts 1-3 arguments"

and try_move_attack brd st num region1 region2 n =
  let result = State.move !st brd region1 region2 n in
  match result with
  | Legal new_st -> st := new_st
  | Illegal -> print_wrong_arg num "troops cannot be moved"

and process_move_attack_full obj_phr brd st num =
  match int_of_string_opt num with
  | Some n -> (
      match State.acting_phase !st with
      | AttackMove (region1, region2, min_num) ->
          try_move_attack brd st num region1 region2 n
      | _ -> failwith "Precondition violated")
  | None -> print_wrong_arg num "is not an integer"

(** [process_move_attack obj_phr brd st] processes when the user uses
    the "move" command during phase AttackMove with non-empty object
    phrase [obj_phr]. *)
and process_move_attack obj_phr brd st =
  match obj_phr with
  | [ num ] | [ _; _; num ] ->
      process_move_attack_full obj_phr brd st num
  | _ ->
      print_error "in the attack phase, move accepts 1 or 3 arguments"

(** [process_end brd st] processes when the user uses the "end" command
    with empty object phrase. *)
and process_end brd st =
  if State.can_end_phase !st then State.end_phase !st brd
  else if State.acting_phase !st = Place then
    print_error
      "you cannot end the Place phase without placing all troops, or \
       with more than 4 cards"
  else
    print_error "after capturing a region, you must move troops into it"

(** [process_show brd st] processes when the user uses the "show"
    command with an empty object phrase. *)
and process_show brd st =
  if not !using_gui then
    let rec string_of_trp_map map =
      match map with
      | [] -> ""
      | (region, player, troops) :: t ->
          region ^ ": " ^ string_of_int troops ^ " troops: "
          ^ Player.name player ^ "\n" ^ string_of_trp_map t
    in
    print_string (string_of_trp_map (State.region_player_troop !st))

and place_info =
  "place [REGION] (NUM) - increases troops in REGION by NUM"

and trade_info =
  "trade - exchanges cards for troops if possible, retaining wildcards \
   if possible"

and attack_info =
  "attack [REGION1] (REGION2) (NUM) - attacks from REGION1 to REGION2 \
   with NUM dice"

and move_info =
  "move [REGION1] (REGION2) (NUM) - moves NUM troops from REGION1 to \
   REGION2"

and end_info = "end - ends the current phase"

and show_info = "show - displays current game state"

and help_info = "help - displays help menu for the current phase"

and quit_info = "quit - ends game"

(** [process_help brd st] processes when the user uses the "help"
    command with an empty object phrase. *)
and process_help brd st =
  let phaseless = [ show_info; help_info; quit_info ] in
  ANSITerminal.print_string [ ANSITerminal.green ] "Help Menu:\n";
  print_string
    "Note: [ARG] is a required argument and (ARG) is an optional one \
     (prompted if not given).\n\
     In the current phase, you can use the following commands:\n\n";
  (match State.acting_phase !st with
  | Preturn ->
      () (* player cannot enter "help" command in Preturn phase *)
  | Place ->
      print_strings ([ place_info; trade_info; end_info ] @ phaseless)
  | Attack -> print_strings ([ attack_info; end_info ] @ phaseless)
  | AttackMove (reg_from, reg_to, min) ->
      print_strings ([ move_info ] @ phaseless)
  | Fortify -> print_strings ([ move_info; end_info ] @ phaseless));
  print_endline ""

(** [process_quit brd st] processes when the user uses the "quit"
    command with an empty object phrase. *)
and process_quit brd st =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Thanks for playing!\n";
  exit 0

(** [process_bad_command brd st] processes when the user inputs a
    malformed or empty command. *)
and process_bad_command brd st =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid command. See ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "help";
  ANSITerminal.print_string [ ANSITerminal.red ] ". \n\n"

(** [prompt_num_players board] prompts the user for and is the number of
    players the game will contain. Continues to prompt until a valid
    number of players, determined by [board], is given. *)
let rec prompt_num_players board =
  print_prompt "How many players do you want in this game?";
  match read_line_and_print () with
  | exception End_of_file -> prompt_num_players board
  | num_input -> num_players_of_input board num_input

and num_players_of_input board num_in =
  match int_of_string_opt num_in with
  | Some x ->
      if x < Board.min_players board then (
        print_wrong_arg num_in "is too few players";
        prompt_num_players board)
      else if x > Board.max_players board then (
        print_wrong_arg num_in "is too many players";
        prompt_num_players board)
      else x
  | None ->
      print_wrong_arg num_in "is not an integer";
      prompt_num_players board

let rec prompt_num_ai_players total =
  print_prompt "How many of the players do you want to be AIs?";
  match read_line_and_print () with
  | exception End_of_file -> prompt_num_ai_players total
  | num_input -> num_ai_players_of_input total num_input

and num_ai_players_of_input total num_in =
  match int_of_string_opt num_in with
  | Some x ->
      if x < 0 then (
        print_wrong_arg num_in "is an invalid number of AIs";
        prompt_num_ai_players total)
      else if x > total then (
        print_wrong_arg num_in "is too many AIs";
        prompt_num_ai_players total)
      else x
  | None ->
      print_wrong_arg num_in "is not an integer";
      prompt_num_ai_players total

(** [prompt_num_players n] prompts the user for AI difficulties until
    [n] are specified. Returns the list of inputted difficulties. *)
let rec prompt_ai_difficulties n =
  if n <= 0 then []
  else (
    print_prompt "Input difficulty (1-2-3) for the next AI.";
    let str_in = read_line_and_print () in
    let num_in =
      match str_in with "1" -> 1 | "2" -> 2 | "3" -> 3 | _ -> 1
    in
    num_in :: prompt_ai_difficulties (n - 1))

(** [prompt_num_players n] prompts the user for player names until [n]
    are specified. Returns the list of inputted names. *)
let rec prompt_player_names n =
  if n <= 0 then []
  else (
    print_prompt "Input a player's name";
    let name = read_line_and_print () in
    name :: prompt_player_names (n - 1))

(** [player_int_list num ai_list] is a list of size [num] with all
    entries in [ai_list] combined with entries of 0.

    Requires: [num] is at least equal to the length of [ai_list]. *)
let rec player_int_list num ai_list =
  if List.length ai_list < num then player_int_list num (0 :: ai_list)
  else ai_list

(** [load_board f] is the board from file [f]. *)
let load_board f =
  print_endline "Loading map...";
  let json =
    match Yojson.Basic.from_file f with
    | exception Sys_error _ ->
        print_endline
          "Error: the map you entered has not been implemented.";
        exit 0
    | x ->
        print_endline "Done!\n";
        x
  in
  Board.from_json json

let print_instructions () =
  ANSITerminal.print_string [ ANSITerminal.green ] "Instructions:\n";
  print_string
    "The map has been initialized with troops in every region. You may \
     play through either a GUI or the terminal. \n\n\
     If you play through the terminal, use commands to perform actions \
     in the game. For more details on commands, use command \"";
  print_string_yellow "help";
  print_endline "\".\n";
  print_string
    "To end the game, either close the display or use command \"";
  print_string_yellow "quit";
  print_endline "\".\n"

(** [prompt_use_gui ()] prompts the user for and is whether the GUI will
    be used to play the game. Continues to prompt until "yes" or "no" is
    specified. *)
let rec prompt_use_gui () =
  print_prompt "Do you want to play using the GUI? (yes/no)";
  match read_line_and_print () with
  | exception End_of_file -> prompt_use_gui ()
  | "yes" -> true
  | "no" -> false
  | _ -> prompt_use_gui ()

(** [play_game f] plays the game using the board in file [f]. *)
let play_game f =
  let board = load_board f in
  let num_players = prompt_num_players board in
  let num_ai_players = prompt_num_ai_players num_players in
  let ai_difficulties = prompt_ai_difficulties num_ai_players in
  print_endline
    ("Creating game with " ^ string_of_int num_players ^ " players...");
  let players = player_int_list num_players ai_difficulties in
  let state = ref (State.init_state_ai board players) in
  print_endline "Done!\n";
  print_instructions ();
  using_gui := prompt_use_gui ();
  if !using_gui then Gui.init board !state;
  take_command board state

(** [file_of_map map_name] is the json file corresponding to the Board
    called [map_name] *)
let file_of_map map_name =
  match map_name with
  (* | "Four Islands" -> "officialmap.json" *)
  | "Classic Risk" -> "classic.json"
  | _ ->
      print_endline "Error: the map you entered does not exist";
      exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Welcome to Risk!\n\
     Created by Shubham G., Uday K., Nikhil K., and Ryan X.\n\n";
  print_endline "Available maps:\n- Classic Risk (2-6 Players)\n";
  print_prompt "Enter the name of a map to play.";
  match read_line_and_print () with
  | exception End_of_file -> ()
  | map_name -> play_game (file_of_map map_name)

let () = main ()
