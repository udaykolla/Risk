(* Test Plan: The features of this project which we have tested entirely
   automatically include the board, card, player, and command
   compilation units. We tested some aspects of the state compilation
   unit automatically, including the resulting troop counts after troops
   have been moved or placed, as well as testing illegal conditions
   while trading and attacking, and lengths of certain fields we
   implemented as lists. We manually tested the attacking capabilities
   of the state compilation unit, as well as the entire dispatcher and
   GUI compilation units, as well as the main.ml file. We mainly
   implemented a black-box approach while testing the components of the
   system. All components of the system have been tested in such a way
   that ensures their correctness - for certain components of the
   software (attacking, AI, GUI) we actually played the game to see if
   they functioned as intended. Additionally, we had AI players play the
   game against each other, to completion. We believed this was the best
   way to make sure the software functions properly - testing in a
   real-time, practical setting. We ensured correctness of the remaining
   determinable components through thorough automatic OUnit testing, to
   ensure that they function as intended, in any given case. *)

open OUnit2
open Board
open Command
open State

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_bool b = if b then "true" else "false"

let pp_int i = i

let test_int (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~printer:string_of_int

let test_str (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~printer:pp_string

let test_bool (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~printer:pp_bool

let test_str_lst (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~cmp:cmp_set_like_lists
    ~printer:(pp_list pp_string)

let test_int_lst (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~cmp:cmp_set_like_lists
    ~printer:(pp_list string_of_int)

let test_lst (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~cmp:cmp_set_like_lists

let test_list_tup (name : string) real_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output real_output ~cmp:cmp_set_like_lists

let test_equals (name : string) real_output expected_output : test =
  name >:: fun _ -> assert_equal expected_output real_output

let command_test
    (name : string)
    (input : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse input)

let command_test_exc (name : string) (input : string) exc : test =
  name >:: fun _ -> assert_raises exc (fun () -> parse input)

let board_tests =
  let board = from_json (Yojson.Basic.from_file "officialmap.json") in
  [
    test_str_lst "region_ids test" (region_ids board)
      [
        "A1";
        "A2";
        "A3";
        "A4";
        "B1";
        "B2";
        "B3";
        "B4";
        "C1";
        "C2";
        "C3";
        "C4";
        "D1";
        "D2";
        "D3";
        "D4";
      ];
    test_list_tup "coordinates test" (coordinates board)
      [
        ("A1", (0, 0));
        ("A2", (0, 0));
        ("A3", (0, 0));
        ("A4", (0, 0));
        ("B1", (0, 0));
        ("B2", (0, 0));
        ("B3", (0, 0));
        ("B4", (0, 0));
        ("C1", (0, 0));
        ("C2", (0, 0));
        ("C3", (0, 0));
        ("C4", (0, 0));
        ("D1", (0, 0));
        ("D2", (0, 0));
        ("D3", (0, 0));
        ("D4", (0, 0));
      ];
    test_int "num_regions is 16" (num_regions board) 16;
    test_bool "A3 is a region" (is_region board "A3") true;
    test_bool "E1 is not a region" (is_region board "E1") false;
    test_str "description of D3 is Madagascar"
      (region_description board "D3")
      "Madagascar";
    test_str_lst "neighbors of B3 is B1, B4, A4, D1"
      (neighbors board "B3")
      [ "B1"; "B4"; "A4"; "D1" ];
    test_str_lst "continent_ids is A, B, C, D" (continent_ids board)
      [ "A"; "B"; "C"; "D" ];
    test_str "description of C is Europe"
      (continent_description board "C")
      "Europe";
    test_int "continent bonus of D is 2" (continent_bonus board "D") 2;
    test_str_lst "regions of A are A1, A2, A3, A4"
      (regions_of_continent board "A")
      [ "A1"; "A2"; "A3"; "A4" ];
    test_str "continent of D2 is D" (continent_of_region board "D2") "D";
    test_bool "D1 is a neighbor of C2"
      (is_neighbor board "C2" "D1")
      true;
    test_bool "D3 is not neighbor of C2"
      (is_neighbor board "C2" "D3")
      false;
    test_int "minimum number of players is 2" (min_players board) 2;
    test_int "maximum number of players is 6" (max_players board) 2;
    test_int_lst "troops per player is [40]"
      (troops_per_player board)
      [ 40 ];
  ]

let command_tests =
  [
    command_test
      {|parse "     place   Alberta 4" is Place ["Alberta"; "4"]|}
      "     place   Alberta 4"
      (Place [ "Alberta"; "4" ]);
    command_test
      {|parse "   move   Alberta   Ontario 3   " is Move ["Alberta"; "Ontario"; "3"]|}
      "   move   Alberta   Ontario 3   "
      (Move [ "Alberta"; "Ontario"; "3" ]);
    command_test
      {|parse " attack     India China   2 " is Attack ["India"; "China"; "2"]|}
      " attack     India China   2 "
      (Attack [ "India"; "China"; "2" ]);
    command_test {|parse "help" is Help|} "help" Help;
    command_test {|parse "     quit " is Quit|} "     quit " Quit;
    command_test {|parse "end   " is End|} "end   " End;
    command_test {|parse "   trade   " is Trade|} "   trade   " Trade;
    command_test {|parse "show" is Show|} "show" Show;
    command_test_exc {|parse "     " is Empty|} "     " Empty;
    command_test_exc {|parse "" is Empty|} "" Empty;
    command_test_exc {|parse " move   " is Malformed|} " move   "
      Malformed;
    command_test_exc {|parse "help please" is Malformed|} "help please"
      Malformed;
    command_test_exc {|parse "go   " is Malformed|} "go   " Malformed;
  ]

let player = Player.init_player "Shubham" [] Graphics.blue Player.Human

let player2 =
  Player.init_player "Nikhil" [ "A1" ] Graphics.magenta (Player.AI 2)

let card = Card.init_card Infantry "A1"

let () = Player.add_card player card

let () = Player.set_cards player2 [ card; card ]

let () = Player.remove_region player2 "A1"

let () = Player.add_region player2 "B1"

let () = Player.set_regions player [ "A1"; "A2" ]

let () = Player.set_unplaced_troops player2 3

let player_tests =
  [
    test_str "name of player is Shubham" (Player.name player) "Shubham";
    test_str "name of player2 is Nikhil" (Player.name player2) "Nikhil";
    test_str_lst "player has no regions" (Player.regions player)
      [ "A1"; "A2" ];
    test_str_lst "player2 has one region"
      (Player.regions player2)
      [ "B1" ];
    test_equals "color of player is blue" (Player.color player)
      Graphics.blue;
    test_equals "color of player2 is magenta" (Player.color player2)
      Graphics.magenta;
    test_bool "player does have region A1"
      (Player.has_region player "A1")
      true;
    test_bool "player does have region A2"
      (Player.has_region player "A2")
      true;
    test_bool "player2 doesn't have region A1"
      (Player.has_region player2 "A1")
      false;
    test_bool "player2 does have region B1"
      (Player.has_region player2 "B1")
      true;
    test_lst "player has one card" (Player.cards player) [ card ];
    test_int "player2 has two cards"
      (List.length (Player.cards player2))
      2;
    test_int "player has no unplaced troops"
      (Player.unplaced_troops player)
      0;
    test_int "player2 has 3 unplaced troops"
      (Player.unplaced_troops player2)
      3;
    test_equals "player is a human"
      (Player.player_type player)
      Player.Human;
    test_equals "player2 is a human"
      (Player.player_type player2)
      (Player.AI 2);
  ]

let wildcard = Card.init_card Wildcard ""

let card_tests =
  [
    test_equals "card has picture Infantry" (Card.picture card)
      Card.Infantry;
    test_equals "wildcard has picture Wildcard" (Card.picture wildcard)
      Card.Wildcard;
    test_str "card has region A1" (Card.region card) "A1";
    test_str "wildcard has no region" (Card.region wildcard) "";
    test_str "string representation of card is A1 - Infantry"
      (Card.string_of_card card)
      "A1 - Infantry";
    test_str "string representation of wildcard is Wildcard"
      (Card.string_of_card wildcard)
      "Wildcard";
  ]

let board = from_json (Yojson.Basic.from_file "officialmap.json")

let state =
  let to_return = init_state board 2 in
  to_return

let acting_player_regions = Player.regions (acting_player state)

let rec region2_helper neighbors =
  match neighbors with
  | [] -> (false, "")
  | h :: t ->
      if Player.has_region (acting_player state) h then (true, h)
      else region2_helper t

let rec region_helper regions =
  match regions with
  | [] -> ("", "")
  | h :: t ->
      let result = region2_helper (Board.neighbors board h) in
      if fst result then (h, snd result) else region_helper t

let regions = region_helper acting_player_regions

let region1 = fst regions

let region2 = snd regions

let get_initial_state st =
  init_state_with_args (State.reg_tr_map st) (State.acting_phase st)
    (State.turn_order st) (State.get_deck st) (State.get_trades st)
    (State.territory_captured st)

let initial_state = ref (get_initial_state state)

let get_state d =
  match d with
  | Legal t ->
      initial_state := get_initial_state t;
      t
  | Illegal -> state

let example_list = [ 1; 2; 3; 4 ]

let state_tests =
  [
    test_list_tup
      "shuffling a list is the same list in a different order"
      (shuffle example_list) example_list;
    test_int "state_region_troop_map"
      (List.length (reg_tr_map state))
      16;
    test_int "troops in region1 are accurate"
      (List.assoc region1 (reg_tr_map state))
      (troop_count state region1);
    test_int "troops in region2 are accurate"
      (List.assoc region2 (reg_tr_map state))
      (troop_count state region2);
    test_int "number of cards in deck is 18 initially"
      (List.length (get_deck state))
      18;
    test_int "deck length equality"
      (List.length (get_deck state))
      (List.length (Board.region_ids board) + 2);
    test_int "number of players is 2" (List.length (turn_order state)) 2;
    test_int "number of trade is 0 initially" (get_trades state) 0;
    test_bool "territory captured" (territory_captured state) false;
    test_bool "state_not_constant_troops"
      (List.for_all
         (fun x -> troop_count state x = 2)
         (region_ids board))
      false;
    test_int "state_place"
      (troop_count (get_state (place state region1 1)) region1)
      (troop_count !initial_state region1 + 1);
    test_int "state_move"
      (troop_count
         (get_state (move state board region1 region2 1))
         region1)
      (troop_count !initial_state region1 - 1);
    test_equals "the phase initially should be Preturn"
      (acting_phase state) Preturn;
    test_bool "in phase Preturn, can_end_phase is true"
      (can_end_phase state) true;
    test_str "string of hand is empty string initially"
      (string_of_hand state) "";
    test_int "region player troops list"
      (List.length (region_player_troop state))
      16;
    test_equals "trade should be illegal initially" (State.trade state)
      State.Illegal;
    test_equals
      "attack of two neighboring regions owned by same player is \
       illegal"
      (State.attack state board region1 region2 3)
      State.Illegal;
    test_bool "region1 is owned by current player"
      (State.owned_by_current state region1)
      true;
    test_bool "region2 is owned by current player"
      (State.owned_by_current state region2)
      true;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           board_tests;
           command_tests;
           player_tests;
           card_tests;
           state_tests;
         ]

let _ = run_test_tt_main suite
