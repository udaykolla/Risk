open Graphics
open Camlimages
open State
open Player
open Board

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

let counter = ref 0

let cmd_box_wdth = 50

let cmd_box_ht = 25

let cnt_box_dim = 25

let cmd_box_spacer = 75

let txt_indent = 5

let ply_txt_indent = 10

let cnt_box_spacer = 60

let top_box_coord = (1060, 650)

let plr_bx_wdth = 100

let ply_bx_ht = 25

let ply_bx_spacer = 50

let circle_spacer = 85

let plyr_circle_radius = 8

let terr_circle_radius = 10

let check_bounds x y x1 y1 x2 y2 =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

let command_boxes x y s1 s2 hl1 hl2 =
  set_color hl1;
  fill_rect x y cmd_box_wdth cmd_box_ht;
  set_color hl2;
  fill_rect (x + cmd_box_spacer) y cmd_box_wdth cmd_box_ht;
  set_color black;
  moveto (x + txt_indent) y;
  draw_string s1;
  moveto (x + cmd_box_spacer + txt_indent) y;
  draw_string s2

let counter_boxes x y i =
  counter := i;
  set_color cyan;
  fill_rect x y cnt_box_dim cnt_box_dim;
  set_color white;
  fill_rect (x + cnt_box_spacer) y cnt_box_dim cnt_box_dim;
  set_color cyan;
  fill_rect (x + (2 * cnt_box_spacer)) y cnt_box_dim cnt_box_dim;
  set_color black;
  moveto (x + txt_indent) y;
  draw_string "+";
  moveto (x + 65) y;
  let counter_string = if i < 0 then "-" else string_of_int i in
  draw_string counter_string;
  moveto (x + 125) y;
  draw_string "-"

let f a = Float.of_int a

let terr_find x y b c =
  Float.sqrt (((f x -. f b) ** 2.) +. ((f y -. f c) ** 2.)) <= 10.

let list_brk l x y = match l with a, (b, c) -> terr_find x y b c

let rec terr_find x y list =
  match list with
  | [] -> Nothing
  | h :: k -> if list_brk h x y then Region (fst h) else terr_find x y k

let assign_click (x : int) (y : int) brd : click =
  if check_bounds x y 1050 185 1100 210 then Place
  else if check_bounds x y 1125 185 1175 210 then Trade
  else if check_bounds x y 1050 125 1100 150 then Attack
  else if check_bounds x y 1125 125 1175 150 then Move
  else if check_bounds x y 1050 65 1100 90 then Enter
  else if check_bounds x y 1125 65 1175 90 then End
  else if check_bounds x y 1041 240 1066 265 then Plus
  else if check_bounds x y 1161 240 1186 265 then Minus
  else terr_find x y (Board.coordinates brd)

let coord brd terr = List.assoc terr (Board.coordinates brd)

let fill_in brd terr p player =
  let cor = coord brd terr in
  let x = fst cor in
  let y = snd cor in
  set_color (Player.color player);
  draw_circle x y terr_circle_radius;
  set_color white;
  moveto (x - 2) (y - 5);
  draw_string (string_of_int (State.troop_count p terr))

let rec territories
    (brd : Board.t)
    (terr : Board.region_id list)
    (state : State.t)
    (player : Player.t) =
  match terr with
  | [] -> ()
  | h :: k ->
      fill_in brd h state player;
      territories brd k state player

let win_lose_state a =
  set_color white;
  fill_rect 1060 650 100 25;
  set_color blue;
  moveto 1080 655;
  if a then draw_string "Game Over!" else draw_string "Turn Order :"

let draw_player_box counter h =
  let x = fst top_box_coord in
  let y = snd top_box_coord - (ply_bx_spacer * counter) in
  set_color white;
  fill_rect x y plr_bx_wdth ply_bx_ht;
  set_color black;
  moveto (x + ply_txt_indent) (y + txt_indent);
  draw_string (Player.name h);
  set_color (Player.color h);
  fill_circle (x + circle_spacer) (y + ply_txt_indent)
    plyr_circle_radius

let rec multiple_players (list : Player.t list) (counter : int) =
  match list with
  | [] -> ()
  | h :: k ->
      draw_player_box counter h;
      multiple_players k (counter + 1)

let draw_phase s =
  set_color white;
  fill_rect 1060 300 100 25;
  set_color blue;
  moveto 1075 305;
  draw_string ("Phase: " ^ s)

let rec players (brd : Board.t) (list : Player.t list) (state : State.t)
    =
  match list with
  | [] -> ()
  | h :: k ->
      territories brd (Player.regions h) state h;
      players brd k state

let draw_names (list : Player.t list) (counter : int) =
  win_lose_state (List.length list = 1);
  multiple_players list counter

let player_count state = State.acting_player state |> unplaced_troops

let counter_init_val st =
  match State.acting_phase st with
  | Preturn -> 0
  | Place -> player_count st
  | Attack -> max 1 !counter
  | AttackMove (_, _, min_num) -> min_num
  | Fortify -> max 1 !counter

let player_phase state =
  match state |> acting_phase with
  | Preturn | Place -> "Place"
  | Attack | AttackMove _ -> "Attack"
  | Fortify -> "Fortify"

let board_outline (pl_col, tr_col, at_col, mo_col, et_col, en_col) =
  let map = Jpeg.load "riskmap.jpg" [] in
  Graphic_image.draw_image map 0 0;
  set_color blue;
  fill_rect 1021 0 189 689;
  set_color white;
  command_boxes 1050 185 "Place" "Trade" pl_col tr_col;
  command_boxes 1050 125 "Attack" "Move" at_col mo_col;
  command_boxes 1050 65 "Enter" "End" et_col en_col

let get_cols st =
  match State.acting_phase st with
  | Preturn -> (white, white, white, white, green, green)
  | Place -> (green, green, white, white, green, green)
  | Attack -> (white, white, green, white, green, green)
  | AttackMove _ -> (white, white, white, green, green, white)
  | Fortify -> (white, white, white, green, green, green)

let update (brd : Board.t) (state : State.t) =
  get_cols state |> board_outline;
  draw_names (State.turn_order state) 1;
  players brd (State.turn_order state) state;
  counter_boxes 1041 240 (counter_init_val state);
  draw_phase (state |> player_phase)

let display_message s color brd st =
  update brd st;
  set_color color;
  moveto 0 0;
  draw_string s

let show_current brd st s = display_message s blue brd st

let show_error brd st s = display_message s red brd st

let next_click brd : click =
  let e = wait_next_event [ Button_down ] in
  assign_click e.mouse_x e.mouse_y brd

let set_counter i =
  counter := i;
  counter_boxes 1041 240 i

let get_counter () = !counter

(** [append_counter obj_phr] is the current value of the counter
    appended to [obj_phr]. *)
let append_counter obj_phr =
  obj_phr @ [ string_of_int (get_counter ()) ]

let show_wrong_arg brd st arg msg =
  show_error brd st ("Error: " ^ arg ^ " " ^ msg)

(** [in_prog_counter_attack brd st obj_phr] sets the counter according
    to the in-progress attack command composed with object phrase
    [obj_phr]. *)
let in_prog_counter_attack st obj_phr =
  match obj_phr with
  | [ r ] -> State.troop_count st r - 1 |> min 3 |> set_counter
  | _ -> ()

(** [in_prog_counter_move brd st obj_phr] sets the counter according to
    the in-progress move command composed with object phrase [obj_phr]. *)
let in_prog_counter_move st obj_phr =
  match obj_phr with
  | [ r ] -> State.troop_count st r - 1 |> set_counter
  | _ -> ()

(** [show_in_prog brd st cl obj_phr] displays the in-progress command
    composed of [cl] and [obj_phr].

    Requires: [cl] is Nothing, Place, Attack, Move, Trade, or End *)
let show_in_prog brd st cl obj_phr =
  let spaced_op = String.concat " " obj_phr in
  let show_concat_op s = show_current brd st (s ^ " " ^ spaced_op) in
  match cl with
  | Nothing -> show_current brd st ""
  | Place -> show_concat_op "Place"
  | Attack ->
      show_concat_op "Attack";
      in_prog_counter_attack st obj_phr
  | Move ->
      show_concat_op "Move";
      in_prog_counter_move st obj_phr
  | Trade -> show_concat_op "Trade"
  | End -> show_concat_op "End"
  | Region _ | Plus | Minus | Enter -> failwith "Precondition violated"

(** [do_if_false b f] executes [f] if [b] is false and returns [b]. *)
let do_if_false b f =
  if b then true
  else (
    f ();
    false)

(** [check_owned_by_current st id] is whether region [id] is owned by
    the acting player in [st].

    Effects: if not, displays error message. *)
let check_owned_by_current brd st id =
  do_if_false (State.owned_by_current st id) (fun () ->
      show_wrong_arg brd st id "is not your region")

(** [check_neighbor brd r id] is whether [id] is a region neighboring
    region [r].

    Effects: if not, displays error message. *)
let check_neighbor brd st r id =
  do_if_false (Board.is_neighbor brd r id) (fun () ->
      show_wrong_arg brd st id ("does not neighbor " ^ r))

(** [check_spare_troop st id] is whether region [id] has at least 2
    troops in [st].

    Effects: if not, displays error message. *)
let check_spare_troop brd st id =
  do_if_false
    (State.troop_count st id >= 2)
    (fun () -> show_wrong_arg brd st id "only has 1 troop")

(** [process_place brd st obj_phr id] is whether a valid "place" command
    could be formed with object phrase beginning with the concatenation
    of [obj_phr] and [id].

    Effects: if false, displays error. *)
let process_place brd st obj_phr id =
  match obj_phr with
  | [] -> check_owned_by_current brd st id
  | _ ->
      show_error brd st "Place needs 1 region specified";
      false

(** [process_attack brd st obj_phr id] is whether a valid "attack"
    command could be formed with object phrase beginning with the
    concatenation of [obj_phr] and [id].

    Effects: if false, displays error. *)
let process_attack brd st obj_phr id =
  match obj_phr with
  | [] ->
      check_owned_by_current brd st id && check_spare_troop brd st id
  | [ r1 ] ->
      let check_owner_attack () =
        do_if_false
          (State.owned_by_current st id |> not)
          (fun () ->
            show_wrong_arg brd st id
              "is your territory so you cannot attack it")
      in
      check_neighbor brd st r1 id && check_owner_attack ()
  | _ ->
      show_error brd st "Attack needs 2 regions specified";
      false

(** [process_move brd st obj_phr id] is whether a valid "move" command
    could be formed with object phrase beginning with the concatenation
    of [obj_phr] and [id].

    Effects: if false, displays error. *)
let process_move brd st obj_phr id =
  match obj_phr with
  | [] -> check_owned_by_current brd st id
  | [ r1 ] ->
      let check_owner_move () =
        do_if_false (State.owned_by_current st id) (fun () ->
            show_wrong_arg brd st id
              "is not your territory so you cannot move there")
      in
      check_neighbor brd st r1 id && check_owner_move ()
  | _ ->
      show_error brd st "Move needs 2 regions specified";
      false

(** [get_command_helper brd st cl obj_phr] is the command corresponding
    to the series of user clicks since the previous command was
    processed. *)
let rec get_command_helper brd st cl obj_phr =
  match next_click brd with
  | Nothing -> get_command_helper brd st cl obj_phr
  | Region id -> get_command_region brd st cl obj_phr id
  | Plus -> get_command_plus brd st cl obj_phr
  | Minus -> get_command_minus brd st cl obj_phr
  | Enter -> get_command_enter brd st cl obj_phr
  | Place -> get_command_place brd st cl obj_phr
  | Attack -> get_command_attack brd st cl obj_phr
  | Move -> get_command_move brd st cl obj_phr
  | Trade -> get_command_trade brd st cl obj_phr
  | End -> get_command_end brd st cl obj_phr

(** [get_command_region brd st cl obj_phr id] is the command
    corresponding to the series of user clicks since the previous
    command was processed, with a click on region [id] being most
    recent.

    Effects: if [id] could not lead to a valid command when attached to
    [cl] and [obj_phr], displays error. If it could, displays [cl] and
    [obj_phr], the command "in progress". *)
and get_command_region brd st cl obj_phr id =
  let op_with_id = obj_phr @ [ id ] in
  let process_with f =
    if f brd st obj_phr id then (
      show_in_prog brd st cl op_with_id;
      get_command_helper brd st cl op_with_id)
    else get_command_helper brd st Nothing []
  in
  match cl with
  | Nothing -> get_command_helper brd st cl obj_phr
  | Place -> process_with process_place
  | Attack -> process_with process_attack
  | Move -> process_with process_move
  | Trade | End -> get_command_helper brd st cl obj_phr
  | Region _ | Plus | Minus | Enter -> failwith "unreachable branch"

and max_counter_attack st obj_phr =
  match obj_phr with
  | [ r ] | [ r; _ ] -> State.troop_count st r - 1 |> min 3
  | _ -> 3

and max_counter_fortify st obj_phr =
  match obj_phr with
  | [ r ] | [ r; _ ] -> State.troop_count st r - 1
  | _ -> 1000

(** [max_counter brd st cl obj_phr] is the value above which commands
    beginning with [cl] and [obj_phr] would always fail. *)
and max_counter brd st cl obj_phr =
  match State.acting_phase st with
  | Preturn -> failwith "Unreachable branch"
  | Place -> player_count st
  | Attack -> max_counter_attack st obj_phr
  | AttackMove (r_from, _, _) -> State.troop_count st r_from - 1
  | Fortify -> max_counter_fortify st obj_phr

and get_command_plus brd st cl obj_phr =
  let ceiling = max_counter brd st cl obj_phr in
  let cur = get_counter () in
  min ceiling (cur + 1) |> set_counter;
  get_command_helper brd st cl obj_phr

and get_command_minus brd st cl obj_phr =
  (match State.acting_phase st with
  | AttackMove (_, _, min_num) ->
      let cur = get_counter () in
      max min_num (cur - 1) |> set_counter
  | _ ->
      let cur = get_counter () in
      max 0 (cur - 1) |> set_counter);
  get_command_helper brd st cl obj_phr

(** [regions_complete brd st cl obj_phr] is whether the [obj_phr]
    contains the correct number of regions for the command beginning
    with [cl]. *)
and regions_complete brd st cl obj_phr =
  match cl with
  | Nothing | Trade | End -> List.length obj_phr = 0
  | Place -> List.length obj_phr = 1
  | Attack -> List.length obj_phr = 2
  | Move ->
      if State.acting_phase st = Fortify then List.length obj_phr = 2
      else true
  | Region _ | Plus | Minus | Enter -> failwith "unreachable branch"

(** [get_command_enter brd st cl obj_phr] is the command corresponding
    to the series of user clicks since the previous command was
    processed, with "Enter" being most recent. *)
and get_command_enter brd st cl obj_phr =
  if not (regions_complete brd st cl obj_phr) then (
    show_error brd st "not enough regions specified";
    get_command_helper brd st Nothing obj_phr)
  else
    match cl with
    | Nothing -> get_command_helper brd st Nothing obj_phr
    | Place -> Command.Place (append_counter obj_phr)
    | Attack -> Command.Attack (append_counter obj_phr)
    | Move -> Command.Move (append_counter obj_phr)
    | Trade -> Command.Trade
    | End -> Command.End
    | Region _ | Plus | Minus | Enter -> failwith "unreachable branch"

and get_command_place brd st cl obj_phr =
  match State.acting_phase st with
  | Preturn -> failwith "Unreachable branch"
  | Place ->
      show_in_prog brd st Place [];
      get_command_helper brd st Place []
  | Attack | AttackMove _ | Fortify ->
      show_error brd st "cannot use that command now";
      get_command_helper brd st Nothing []

and get_command_attack brd st cl obj_phr =
  match State.acting_phase st with
  | Preturn -> failwith "Unreachable branch"
  | Attack ->
      show_in_prog brd st Attack [];
      get_command_helper brd st Attack []
  | Place | AttackMove _ | Fortify ->
      show_error brd st "cannot use that command now";
      get_command_helper brd st Nothing []

and get_command_move brd st cl obj_phr =
  match State.acting_phase st with
  | Preturn -> failwith "Unreachable branch"
  | Fortify ->
      show_in_prog brd st Move [];
      get_command_helper brd st Move []
  | AttackMove (region1, region2, _) ->
      let implied_op = [ region1; region2 ] in
      show_in_prog brd st Move implied_op;
      get_command_helper brd st Move []
  | Place | Attack ->
      show_error brd st "cannot use that command now";
      get_command_helper brd st Nothing []

and get_command_trade brd st cl obj_phr =
  match State.acting_phase st with
  | Preturn -> failwith "Unreachable branch"
  | Place ->
      show_in_prog brd st Trade [];
      get_command_helper brd st Trade []
  | Attack | AttackMove _ | Fortify ->
      show_error brd st "cannot use that command now";
      get_command_helper brd st Nothing []

and get_command_end brd st cl obj_phr =
  match State.acting_phase st with
  | Preturn -> failwith "Unreachable branch"
  | Place ->
      if State.can_end_phase st then (
        show_in_prog brd st End [];
        get_command_helper brd st End [])
      else (
        show_error brd st
          "cannot end the Place phase without placing all troops, or \
           with more than 4 cards";
        get_command_helper brd st Nothing [])
  | Attack | Fortify ->
      show_in_prog brd st End [];
      get_command_helper brd st End []
  | AttackMove _ ->
      show_error brd st
        "after capturing a region, you must move troops into it";
      get_command_helper brd st Nothing []

let rec get_command brd st = get_command_helper brd st Nothing []

let init (brd : Board.t) (state : State.t) =
  try
    open_graph " 1210x689";
    set_window_title "Risk";
    set_line_width 1;
    update brd state
  with Graphic_failure _ ->
    print_endline
      "The GUI failed to open. Please see install instructions.";
    exit 0
