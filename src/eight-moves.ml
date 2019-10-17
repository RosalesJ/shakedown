open Core

module Eight_Moves = struct
  let moves = Eight_Puzzle.moves

  let left = List.nth_exn moves 0
  let right = List.nth_exn moves 1
  let up = List.nth_exn moves 2
  let down = List.nth_exn moves 3
end
