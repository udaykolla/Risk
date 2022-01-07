open Yojson.Basic.Util

type region_id = string

type continent_id = string

exception UnknownRegion of region_id

exception UnknownContinent of continent_id

type region = {
  rid : region_id;
  description : string;
  neighbors : region_id list;
  coordinates : int * int;
}

type continent = {
  cid : continent_id;
  description : string;
  bonus : int;
  regions : region list;
}

type board = {
  continents : continent list;
  min_p : int;
  max_p : int;
  troops_per_player : int list;
}

type t = board

let sort lst =
  List.sort_uniq
    (fun a b -> if a < b then -1 else if a > b then 1 else 0)
    lst

let region_of_json j =
  {
    rid = j |> member "id" |> to_string;
    description = j |> member "description" |> to_string;
    neighbors = j |> member "neighbors" |> to_list |> List.map to_string;
    coordinates =
      (let x = j |> member "x" |> to_int in
       let y = j |> member "y" |> to_int in
       (x, y));
  }

let continent_of_json j =
  {
    cid = j |> member "continent id" |> to_string;
    description = j |> member "description" |> to_string;
    bonus = j |> member "bonus" |> to_int;
    regions =
      j |> member "regions" |> to_list |> List.map region_of_json;
  }

let from_json j =
  {
    continents =
      j |> member "continents" |> to_list |> List.map continent_of_json;
    min_p = j |> member "min players" |> to_int;
    max_p = j |> member "max players" |> to_int;
    troops_per_player =
      j |> member "troops per player" |> to_list |> List.map to_int;
  }

let find_continent b c =
  let found = List.filter (fun a -> a.cid = c) b.continents in
  if List.length found = 0 then raise (UnknownContinent c)
  else List.hd found

let contains_region c r = List.filter (fun a -> a.rid = r) c.regions

let find_region b r =
  let found =
    List.fold_left (fun a c -> contains_region c r @ a) [] b.continents
  in
  if List.length found = 0 then raise (UnknownRegion r)
  else List.hd found

let region_ids_of_continent c =
  List.fold_left (fun a b -> b.rid :: a) [] c.regions

let region_ids b =
  sort
    (List.fold_left
       (fun a b -> region_ids_of_continent b @ a)
       [] b.continents)

let coordinates_of_continent c =
  List.fold_left (fun a b -> (b.rid, b.coordinates) :: a) [] c.regions

let coordinates b =
  sort
    (List.fold_left
       (fun a b -> coordinates_of_continent b @ a)
       [] b.continents)

let num_regions b = List.length (region_ids b)

let is_region b r =
  try
    ignore (find_region b r);
    true
  with UnknownRegion _ -> false

let region_description b r = (find_region b r).description

let neighbors b r = sort (find_region b r).neighbors

let continent_ids b =
  sort (List.fold_left (fun a b -> b.cid :: a) [] b.continents)

let continent_description b c = (find_continent b c).description

let continent_bonus b c = (find_continent b c).bonus

let regions_of_continent b c =
  sort (region_ids_of_continent (find_continent b c))

let continent_of_region b r =
  let found =
    List.filter
      (fun c -> List.mem r (region_ids_of_continent c))
      b.continents
  in
  if List.length found = 0 then raise (UnknownRegion r)
  else (List.hd found).cid

let is_neighbor b r1 r2 = List.mem r2 (neighbors b r1)

let min_players b = b.min_p

let max_players b = b.max_p

let troops_per_player b = b.troops_per_player
