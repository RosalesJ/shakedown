open Core

let () =
  let module Heuristic = Eight_puzzle.H2 in
  let module Puzzle = Eight_puzzle in

  let module EP = Search.Metric_space (Puzzle) (Heuristic) in
  let module ET = Game.Tools (Puzzle) in

  let random_state = ET.random_state Puzzle.solved_witness 1310931 in
  let heu = Heuristic.heuristic random_state in

  Printf.printf "%s with heuristic %i\n" "Original state:" heu;
  Puzzle.render random_state;

  EP.best_first random_state
  |> function
  | (None, num) -> Printf.printf "Searched %i states and didn't find the solution\n" num
  | (Some path, num) ->
    let len = List.length path in
    Printf.printf "Path length %i with %i states visited\n" len num;
    if len < 40 then
      ET.render_moves path;
    ET.execute path random_state
    |> Puzzle.render
