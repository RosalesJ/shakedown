open Core
open Eight

let () =
  let module EP = Search.Space(Eight_Puzzle) in
  let module ET = Game.Tools(Eight_Puzzle) in

  let random_state = ET.random_state 133310 in
  let heu = Eight_Puzzle.H.heuristic random_state in

  Printf.printf "%s with heuristic %i\n" "Original state:" heu;
  Eight_Puzzle.render random_state;

  EP.a_star Eight_Puzzle.H.heuristic random_state
  |> function
  | (None, num) -> Printf.printf "Searched %i states and didn't find the solution\n" num
  | (Some path, num) ->
    let len = List.length path in
    Printf.printf "Path length %i with %i states visited\n" len num;
    if len < 40 then
      ET.render_moves path;
    ET.execute path random_state
    |> Eight_Puzzle.render
