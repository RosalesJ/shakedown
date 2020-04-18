open Core

let moves = Eight_puzzle.moves

let left = List.nth_exn moves 0
let right = List.nth_exn moves 1
let up = List.nth_exn moves 2
let down = List.nth_exn moves 3
