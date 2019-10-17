open Core
open Eight
open Search
open Game
open Common


let () =
  let module EP = Search(Eight_Puzzle) in
  let module ET = Game_Tools(Eight_Puzzle) in

  let random_state = ET.random_state 1000 in

  Printf.printf "%s\n" "Original state:";
  Eight_Puzzle.render random_state;

  EP.dfs random_state
  |> function
  | (None, num) -> Printf.printf "Searched %i states and didn't find the solution\n" num
  | (Some path, num) ->
    let len = List.length path in
    Printf.printf "Path length %i with %i states visited\n" len num;
    if len < 40 then
      ET.render_moves path;
    ET.execute path random_state
    |> Eight_Puzzle.render


let () =
  let module GT = Game_Tools(Eight_Puzzle) in
  let open Tuple_Tools in

  let hash_state x = hash 1693 (map_tuple (hash 13) x) in
  let compare a b = (hash_state a) - (hash_state b) in

  let start = ((0, 1, 2), (3, 4, 5), (6, 7, 8)) in
  let s = ((3, 1, 2), (0, 4, 5), (6, 7, 8)) in
  let t = ((3, 1, 2), (6, 4, 5), (0, 7, 8)) in

  Printf.printf "%i\n" (hash_state start);
  Printf.printf "%i\n" (compare s t);
  Printf.printf "%i\n" (hash_state s);
  Printf.printf "%i\n" (hash_state t);
  Tuple_Tools.render (map_tuple (hash 13) start)
