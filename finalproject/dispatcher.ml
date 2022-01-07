(** Flush standard output, then read characters from standard input
    until a newline character is encountered. Return the string of all
    characters read, without the newline character at the end. Then
    print a newline. *)
let read_line_and_print () =
  let x = read_line () in
  print_endline "";
  x

let rec get_command brd st use_gui =
  let pl = State.acting_player st in
  match Player.player_type pl with
  | Human ->
      if use_gui then Gui.get_command brd st
      else Command.parse (read_line_and_print ())
  | AI i -> select_ai i st brd

and select_ai i st b =
  match i with
  | 1 -> ai1 st b
  | 2 -> ai2 st b
  | 3 -> ai3 st b
  | _ -> failwith "unreachable"

and ai1 st b =
  match State.acting_phase st with
  | Preturn -> failwith "unreachable"
  | Place -> place1_helper st
  | Attack -> attack1_helper st b
  | AttackMove d -> attack_move1_helper d
  | Fortify -> fortify1_helper st b

and place1_helper st =
  let player = State.acting_player st in
  let num_cards = List.length (Player.cards player) in
  if num_cards >= 5 then Command.Trade
  else if Player.unplaced_troops player = 0 then Command.End
  else
    let regions = State.shuffle (Player.regions player) in
    Command.Place [ List.hd regions; "1" ]

and attack1_helper st b = Command.End

and attack_move1_helper (move_from, move_to, num_die) =
  Command.Move [ move_from; move_to; string_of_int num_die ]

and fortify1_helper st b = Command.End

and ai2 st b =
  match State.acting_phase st with
  | Preturn -> failwith "unreachable"
  | Place -> place2_helper st
  | Attack -> attack2_helper st b true
  | AttackMove d -> attack_move2_helper d
  | Fortify -> fortify2_helper st b

and ai3 st b =
  match State.acting_phase st with
  | Preturn -> failwith "unreachable"
  | Place -> place3_helper st b
  | Attack -> attack2_helper st b false
  | AttackMove d -> attack_move2_helper d
  | Fortify -> fortify2_helper st b

and num_cards player = List.length (Player.cards player)

and place2_helper st =
  let player = State.acting_player st in
  if num_cards player >= 5 then Command.Trade
  else if Player.unplaced_troops player = 0 then Command.End
  else
    let regions =
      List.sort
        (fun a b -> comparer a b st false)
        (Player.regions player)
    in
    Command.Place [ List.hd regions; "1" ]

and place3_helper_helper_helper regions len player acc =
  match regions with
  | [] -> acc /. len
  | h :: t ->
      if Player.has_region player h then
        place3_helper_helper_helper t len player (acc +. 1.0)
      else place3_helper_helper_helper t len player acc

and place3_helper_helper b continents player acc =
  match continents with
  | [] -> acc
  | h :: t ->
      place3_helper_helper b t player
        (place3_helper_helper_helper
           (Board.regions_of_continent b h)
           (float_of_int (List.length (Board.regions_of_continent b h)))
           player 0.0
         :: acc)

and reg_to_place b player regions continent_with_1 =
  match regions with
  | [] ->
      List.hd
        (Board.regions_of_continent b (fst (List.hd continent_with_1)))
  | h :: t ->
      if Player.has_region player h then h
      else reg_to_place b player t continent_with_1

and sorted_continent_ratio_assoc continent_ratio_assoc =
  List.rev
    (List.sort
       (fun a b ->
         if snd a > snd b then 1 else if snd a < snd b then -1 else 0)
       continent_ratio_assoc)

and cont_ratio_assoc continents ratio_list =
  List.fold_left2
    (fun init a b -> (a, b) :: init)
    [] continents ratio_list

and ratio_list b continents player =
  List.rev (place3_helper_helper b continents player [])

and shuffled_reg b continent =
  State.shuffle (Board.regions_of_continent b continent)

and continent_with_1 sorted_continent_ratio_assoc =
  List.filter (fun a -> snd a = 1.0) sorted_continent_ratio_assoc

and place3_helper st b =
  let player = State.acting_player st in
  if num_cards player >= 5 then Command.Trade
  else if Player.unplaced_troops player = 0 then Command.End
  else
    let continents = Board.continent_ids b in
    let ratio_list = ratio_list b continents player in
    let continent_ratio_assoc =
      cont_ratio_assoc continents ratio_list
    in
    let sorted_assoc =
      sorted_continent_ratio_assoc continent_ratio_assoc
    in
    let sorted_continents_without_1 =
      List.filter (fun a -> snd a <> 1.0) sorted_assoc
    in
    let cont_1 = continent_with_1 sorted_assoc in
    let continent = fst (List.hd sorted_continents_without_1) in
    let regions = shuffled_reg b continent in
    Command.Place [ reg_to_place b player regions cont_1; "1" ]

and attack2_helper st b ai_type =
  let player = State.acting_player st in
  if State.territory_captured st then Command.End
  else
    let regions =
      List.filter
        (fun a -> State.troop_count st a > 1)
        (Player.regions player)
    in
    let regions' =
      List.sort (fun a b -> comparer a b st true) regions
    in
    let result = process_regions regions' st b ai_type in
    match result with None -> Command.End | Some a -> a

(*boolean ai_type = true indicates using AI Level 2, ai_type = false
  indicates using AI Level 3*)
and process_regions regions st b ai_type =
  match regions with
  | [] -> None
  | region :: t ->
      if ai_type then process_neighbors region t st b
      else process_neighbors_3 region t st b

and neighbors_sorted region st b =
  let neighbors =
    List.filter
      (fun a -> not (State.owned_by_current st a))
      (Board.neighbors b region)
  in
  List.sort (fun a b -> comparer a b st false) neighbors

and dice troops = if troops > 3 then 3 else if troops > 2 then 2 else 1

and process_neighbors region t st b =
  let neighbors' = neighbors_sorted region st b in
  if List.length neighbors' = 0 then process_regions t st b true
  else
    let neighbor = List.hd neighbors' in
    let troops = State.troop_count st region in
    Some
      (Command.Attack [ region; neighbor; string_of_int (dice troops) ])

and process_neighbors_3 region t st b =
  let neighbors' = neighbors_sorted region st b in
  let troop_count = State.troop_count st region in
  if List.length neighbors' = 0 then process_regions t st b false
  else
    let least_neighbor = List.hd neighbors' in
    if
      let least_neighbor_troops = State.troop_count st least_neighbor in
      least_neighbor_troops > troop_count
    then process_regions t st b false
    else
      Some
        (Command.Attack
           [ region; least_neighbor; string_of_int (dice troop_count) ])

and comparer a b st decreasing =
  let result = State.troop_count st b - State.troop_count st a in
  if decreasing then result else -result

and attack_move2_helper (move_from, move_to, num_die) =
  Command.Move [ move_from; move_to; string_of_int num_die ]

and fortify2_helper st b =
  let player = State.acting_player st in
  let regions = Player.regions player in
  let data = get_data regions st b in
  if List.length data = 0 then Command.End
  else
    let data' = List.sort fortify2_comparer data in
    match List.hd data' with
    | region1, region2, difference ->
        if difference > 1 then
          Command.Move
            [ region1; region2; string_of_int (difference / 2) ]
        else if difference < -1 then
          Command.Move
            [ region2; region1; string_of_int (-difference / 2) ]
        else Command.End

and get_data regions st b =
  match regions with
  | [] -> []
  | h :: t -> get_data_helper h t st b @ get_data t st b

and get_data_helper region1 regions st b =
  match regions with
  | [] -> []
  | region2 :: t ->
      if Board.is_neighbor b region1 region2 then
        ( region1,
          region2,
          State.troop_count st region1 - State.troop_count st region2 )
        :: get_data_helper region1 t st b
      else get_data_helper region1 t st b

and fortify2_comparer a b =
  match (a, b) with (_, _, x), (_, _, y) -> abs y - abs x
