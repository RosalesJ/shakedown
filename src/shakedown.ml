open Core

module Heuristic = Eight_puzzle.H1
module Puzzle = Eight_puzzle

module EP = Search.Metric_space (Puzzle) (Heuristic)
module ET = Game.Tools (Puzzle)


let test_algo_from_state algo state =
  Puzzle.render state;

  algo state |> function
  | EP.Ded num -> Printf.printf "Searched %i states and didn't find the solution\n" num
  | EP.Solved (solution, num) ->
    let len = List.length solution in
    Printf.printf "Path length %i with %i states visited\n" len num;

    if len < 40 then
      ET.render_moves solution;

    ET.execute solution state
    |> Puzzle.render


let () =
  let random_state = ET.random_state Puzzle.solved_witness 100 in
  let algo = EP.dfs in

  test_algo_from_state algo random_state
