type phase =
  | Preturn
  | Place
  | Attack
  | AttackMove of (Board.region_id * Board.region_id * int)
  | Fortify

type t = {
  mutable region_troop_map : (Board.region_id * int) list;
  mutable ph : phase;
  mutable player_list : Player.t list;
  mutable deck : Card.t list;
  mutable trades : int;
  mutable territory_captured : bool;
}

type result =
  | Legal of t
  | Illegal

let get_deck t = t.deck

let get_trades t = t.trades

let reg_tr_map t = t.region_troop_map

let rec init_state_helper players troops =
  match players with
  | [] -> []
  | player :: t ->
      let num_regions = List.length (Player.regions player) in
      let equal = troops / num_regions in
      let extra = troops mod num_regions in
      init_state_helper2 equal extra (Player.regions player) 0
      @ init_state_helper t troops

and init_state_helper2 equal extra regions n =
  let to_add = if n < extra then 1 else 0 in
  match regions with
  | [] -> []
  | region :: t ->
      (region, equal + to_add)
      :: init_state_helper2 equal extra t (n + 1)

let rec splice a b ind lst =
  match lst with
  | [] -> []
  | h :: t ->
      if ind >= a && ind < b then h :: splice a b (ind + 1) t
      else if ind >= b then []
      else splice a b (ind + 1) t

let share_of_regions n num_players region_list =
  let size = List.length region_list / num_players in
  let num_extra = List.length region_list mod num_players in
  let spliced = splice ((n - 1) * size) (n * size) 0 region_list in
  let extra =
    if n <= num_extra then
      splice
        ((num_players * size) + n - 1)
        ((num_players * size) + n)
        0 region_list
    else []
  in
  spliced @ extra

let color n =
  match n with
  | 1 -> Graphics.red
  | 2 -> Graphics.green
  | 3 -> Graphics.blue
  | 4 -> Graphics.yellow
  | 5 -> Graphics.magenta
  | 6 -> Graphics.black
  | _ -> Graphics.white

let rec player_list_helper n num_players acc regions =
  if n = 0 then acc
  else
    player_list_helper (n - 1) num_players
      (Player.init_player
         ("Player " ^ string_of_int n)
         (share_of_regions n num_players regions)
         (color n) Player.Human
       :: acc)
      regions

let get_player_type ptype =
  match ptype with 0 -> Player.Human | i -> Player.AI i

let get_player_name ptype num_human =
  match ptype with
  | 0 -> "Player " ^ string_of_int num_human
  | i -> "AI Level " ^ string_of_int i

let rec player_list_helper_ai n num_human num_players acc regions ptypes
    =
  if n = 0 then acc
  else
    let ptype = List.hd ptypes in
    let human = ptype = 0 in
    player_list_helper_ai (n - 1)
      (if human then num_human - 1 else num_human)
      num_players
      (Player.init_player
         (get_player_name ptype num_human)
         (share_of_regions n num_players regions)
         (color n) (get_player_type ptype)
       :: acc)
      regions (List.tl ptypes)

let rec make_cards lst =
  let n = List.length lst in
  if n = 0 then []
  else
    match (n mod 3, lst) with
    | 0, h :: t -> Card.init_card Infantry h :: make_cards t
    | 1, h :: t -> Card.init_card Cavalry h :: make_cards t
    | 2, h :: t -> Card.init_card Artillery h :: make_cards t
    | _ -> []

let shuffle lst =
  (* https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
  Random.self_init ();
  let rand = List.map (fun x -> (Random.bits (), x)) lst in
  let sort = List.sort compare rand in
  List.map snd sort

let make_state troops_per_player initial_player_list num_players board =
  {
    region_troop_map =
      init_state_helper initial_player_list troops_per_player;
    ph = Preturn;
    player_list = initial_player_list;
    deck =
      shuffle
        (Card.init_card Wildcard ""
         ::
         Card.init_card Wildcard ""
         :: make_cards (shuffle (Board.region_ids board)));
    trades = 0;
    territory_captured = false;
  }

let init_state board num_players =
  let troops_per_player =
    List.nth (Board.troops_per_player board) (num_players - 2)
  in
  let initial_player_list =
    player_list_helper num_players num_players []
      (shuffle (Board.region_ids board))
  in
  make_state troops_per_player initial_player_list num_players board

let init_state_ai board player_types =
  let num_players = List.length player_types in
  let troops_per_player =
    List.nth (Board.troops_per_player board) (num_players - 2)
  in
  let num_human =
    List.fold_left
      (fun acc x -> if x = 0 then acc + 1 else acc)
      0 player_types
  in
  let initial_player_list =
    player_list_helper_ai num_players num_human num_players []
      (shuffle (Board.region_ids board))
      player_types
  in
  make_state troops_per_player initial_player_list num_players board

let init_state_with_args a b c d e f =
  {
    region_troop_map = a;
    ph = b;
    player_list = c;
    deck = d;
    trades = e;
    territory_captured = f;
  }

let find_player region players =
  List.hd (List.filter (fun x -> Player.has_region x region) players)

let rec rpt_helper map players =
  match map with
  | [] -> []
  | (region, troops) :: t ->
      (region, find_player region players, troops)
      :: rpt_helper t players

let region_player_troop st =
  rpt_helper st.region_troop_map st.player_list

let turn_order t = t.player_list

let acting_player t = List.hd t.player_list

let acting_phase t = t.ph

let placeable_troops t = Player.unplaced_troops (acting_player t)

let troop_count t reg = List.assoc reg t.region_troop_map

let owned_by_current t r = List.mem r (Player.regions (acting_player t))

let owns_all_regions st regions =
  List.for_all (fun x -> owned_by_current st x) regions

let rec total_continent_bonus st b continents =
  match continents with
  | [] -> 0
  | h :: t ->
      if owns_all_regions st (Board.regions_of_continent b h) then
        Board.continent_bonus b h + total_continent_bonus st b t
      else total_continent_bonus st b t

let troop_allocation t b =
  let value =
    (List.length (Player.regions (acting_player t)) / 3)
    + total_continent_bonus t b (Board.continent_ids b)
  in
  if value < 3 then 3 else value

let end_phase t b =
  match t.ph with
  | Preturn ->
      t.ph <- Place;
      Player.set_unplaced_troops (acting_player t)
        (troop_allocation t b)
  | Place -> t.ph <- Attack
  | Attack ->
      t.ph <- Fortify;
      if t.territory_captured then
        if List.length t.deck <> 0 then begin
          Player.add_card (acting_player t) (List.hd t.deck);
          t.deck <- List.tl t.deck
        end
  | AttackMove _ -> t.ph <- Attack
  | Fortify ->
      t.territory_captured <- false;
      t.ph <- Preturn;
      t.player_list <- List.tl t.player_list @ [ List.hd t.player_list ]

let can_end_phase t =
  match acting_phase t with
  | Place ->
      Player.unplaced_troops (acting_player t) = 0
      && List.length (Player.cards (acting_player t)) < 5
  | AttackMove _ -> false
  | _ -> true

let rec string_of_hand_helper cards =
  match cards with
  | [] -> ""
  | [ h ] -> Card.string_of_card h
  | h :: t -> Card.string_of_card h ^ ", " ^ string_of_hand_helper t

let string_of_hand t =
  string_of_hand_helper (Player.cards (acting_player t))

let territory_captured t = t.territory_captured

let rec remove_n st symbol n extra cards =
  if n = 0 then cards
  else
    match cards with
    | [] -> []
    | h :: t ->
        if Card.picture h = symbol then begin
          if owned_by_current st (Card.region h) then extra := 2 else ();
          remove_n st symbol (n - 1) extra t
        end
        else h :: remove_n st symbol n extra t

let bonus t =
  t.trades <- t.trades + 1;
  match t.trades with
  | 1 -> 4
  | 2 -> 6
  | 3 -> 8
  | 4 -> 10
  | 5 -> 12
  | 6 -> 15
  | x -> (5 * x) - 15

let count_cards cards picture =
  List.fold_left
    (fun acc x -> if Card.picture x = picture then acc + 1 else acc)
    0 cards

let remove_cards_diff picture1 picture2 picture3 extra cards t =
  cards
  |> remove_n t picture1 1 extra
  |> remove_n t picture2 1 extra
  |> remove_n t picture3 1 extra
  |> Player.set_cards (acting_player t);
  bonus t + !extra

let remove_cards_same picture extra cards t =
  cards
  |> remove_n t picture 3 extra
  |> Player.set_cards (acting_player t);
  bonus t + !extra

let trade_helper t =
  let extra = ref 0 in
  let cards = Player.cards (acting_player t) in
  let i = count_cards cards Infantry in
  let c = count_cards cards Cavalry in
  let a = count_cards cards Artillery in
  let w = count_cards cards Wildcard in
  if i >= 3 then remove_cards_same Infantry extra cards t
  else if c >= 3 then remove_cards_same Cavalry extra cards t
  else if a >= 3 then remove_cards_same Artillery extra cards t
  else if i >= 1 && c >= 1 && a >= 1 then
    remove_cards_diff Infantry Cavalry Artillery extra cards t
  else if i >= 1 && c >= 1 && w >= 1 then
    remove_cards_diff Infantry Cavalry Wildcard extra cards t
  else if i >= 1 && w >= 1 && a >= 1 then
    remove_cards_diff Infantry Wildcard Artillery extra cards t
  else if w >= 1 && c >= 1 && a >= 1 then
    remove_cards_diff Wildcard Cavalry Artillery extra cards t
  else 0

let trade t =
  let bonus = trade_helper t in
  Player.set_unplaced_troops (acting_player t)
    (placeable_troops t + bonus);
  if bonus = 0 then Illegal else Legal t

let rec place_helper lst reg num_troops acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if fst h = reg then
        place_helper t reg num_troops
          ((fst h, snd h + num_troops) :: acc)
      else place_helper t reg num_troops (h :: acc)

let place t reg num_troops =
  if num_troops <= -1 then Illegal
  else if not (Player.has_region (acting_player t) reg) then Illegal
  else if num_troops > placeable_troops t then Illegal
  else
    let lst =
      List.rev (place_helper t.region_troop_map reg num_troops [])
    in
    t.region_troop_map <- lst;
    Player.set_unplaced_troops (acting_player t)
      (placeable_troops t - num_troops);
    Legal t

let rec move_helper lst move_from move_to num_troops acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if fst h = move_from then
        move_helper t move_from move_to num_troops
          ((fst h, snd h - num_troops) :: acc)
      else if fst h = move_to then
        move_helper t move_from move_to num_troops
          ((fst h, snd h + num_troops) :: acc)
      else move_helper t move_from move_to num_troops (h :: acc)

let attack_move_illegal t num_troops =
  match t.ph with
  | AttackMove (_, _, num_die) -> num_troops < num_die
  | _ -> false

let move t b move_from move_to num_troops =
  if not (Board.is_neighbor b move_from move_to) then Illegal
  else if troop_count t move_from <= num_troops then Illegal
  else if num_troops <= 0 then Illegal
  else if not (Player.has_region (acting_player t) move_from) then
    Illegal
  else if not (Player.has_region (acting_player t) move_to) then Illegal
  else if attack_move_illegal t num_troops then Illegal
  else
    let lst =
      List.rev
        (move_helper t.region_troop_map move_from move_to num_troops [])
    in
    t.region_troop_map <- lst;
    end_phase t b;
    Legal t

let die = [ 1; 2; 3; 4; 5; 6 ]

let rec die_roll nd acc =
  if nd = 0 then acc
  else die_roll (nd - 1) (List.hd (shuffle die) :: acc)

let rec equivalent_length_lists lst2 counter acc =
  if counter = 0 then acc
  else
    match lst2 with
    | [] -> acc
    | h :: t -> equivalent_length_lists t (counter - 1) (h :: acc)

(** if attacker > defender then append 0; if attacker < defender then
    append 1; if attacker = defender then append 2 *)
let comp_lst lst1 lst2 =
  List.fold_left2
    (fun init a b ->
      if a > b then 0 :: init
      else if a < b then 1 :: init
      else 2 :: init)
    [] lst1 lst2

let rec get_map map lst move_from move_to =
  match lst with
  | [] -> map
  | h :: t ->
      if h = 0 then
        let new_troops = List.assoc move_to map - 1 in
        get_map
          ((move_to, new_troops) :: List.remove_assoc move_to map)
          t move_from move_to
      else
        let new_troops = List.assoc move_from map - 1 in
        get_map
          ((move_from, new_troops) :: List.remove_assoc move_from map)
          t move_from move_to

let conditions_false t b move_from move_to num_die =
  troop_count t move_from < 2
  || (not (Board.is_neighbor b move_from move_to))
  || (not (Player.has_region (acting_player t) move_from))
  || Player.has_region (acting_player t) move_to
  || (num_die > 3 || num_die < 1)
  || troop_count t move_from <= num_die

let defender_num_dice t move_to =
  if troop_count t move_to >= 2 then 2 else 1

let def_dice dnd =
  if dnd = 1 then die_roll 1 []
  else List.rev (List.sort compare (die_roll 2 []))

let att_dice num_die =
  List.rev (List.sort compare (die_roll num_die []))

let upd_def_dice defender_dice attacker_dice =
  if List.length defender_dice > List.length attacker_dice then
    List.rev
      (equivalent_length_lists defender_dice
         (List.length attacker_dice)
         [])
  else defender_dice

let upd_att_dice attacker_dice defender_dice =
  if List.length attacker_dice > List.length defender_dice then
    List.rev
      (equivalent_length_lists attacker_dice
         (List.length defender_dice)
         [])
  else attacker_dice

let new_player_list can_elim def t =
  if can_elim then
    List.filter
      (* (fun x -> Player.name x <> Player.name def) *)
        (fun x -> List.length (Player.regions x) != 0)
      t.player_list
  else t.player_list

let set_new_cards act_pl def =
  Player.set_cards act_pl
    (shuffle (Player.cards act_pl @ Player.cards def))

let get_comp_lst t move_to num_die =
  let def_die = defender_num_dice t move_to in
  let upd_def_dice =
    upd_def_dice (def_dice def_die) (att_dice num_die)
  in
  let upd_att_dice =
    upd_att_dice (att_dice num_die) (def_dice def_die)
  in
  List.rev (comp_lst upd_att_dice upd_def_dice)

let attack t b move_fr move_to num_die =
  if conditions_false t b move_fr move_to num_die then Illegal
  else
    let comp_list = get_comp_lst t move_to num_die in
    let new_map =
      get_map t.region_troop_map comp_list move_fr move_to
    in
    let act_pl = acting_player t in
    let def = find_player move_to t.player_list in
    let capt = List.assoc move_to new_map = 0 in
    if capt then Player.add_region act_pl move_to;
    if capt then Player.remove_region def move_to;
    let can_elim = List.length (Player.regions def) = 0 in
    let upd_elim_player_lst = new_player_list can_elim def t in
    if can_elim then set_new_cards act_pl def;
    t.player_list <- upd_elim_player_lst;
    t.region_troop_map <- new_map;
    if capt then t.territory_captured <- true;
    if capt then t.ph <- AttackMove (move_fr, move_to, num_die);
    Legal t
