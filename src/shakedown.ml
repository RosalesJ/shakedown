open Core
open Eight

let () =
  let module Puzzle : Game.H = Eight_H2 in

  let module EP = Search.Metric_Space(Puzzle) in
  let module ET = Game.Tools(Puzzle) in
  let solved = Puzzle.solved_witness in

  let random_state = ET.random_state solved 1310931 in
  let heu = Puzzle.heuristic random_state in

  Printf.printf "%s with heuristic %i\n" "Original state:" heu;
  Puzzle.render random_state;

  EP.best_first Puzzle.heuristic random_state
  |> function
  | (None, num) -> Printf.printf "Searched %i states and didn't find the solution\n" num
  | (Some path, num) ->
    let len = List.length path in
    Printf.printf "Path length %i with %i states visited\n" len num;
    if len < 40 then
      ET.render_moves path;
    ET.execute path random_state
    |> Puzzle.render
