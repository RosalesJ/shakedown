open Core

(*

 -------
| b b b |
| b b b |
| b b b |
 ------- ------- -------
| r r r | w w w | o o o |
| r r r | w w w | o o o |
| r r r | w w w | o o o |
 ------- ------- -------
|
*)

module Tuple_tools = struct
  let map_tuple f = function (a, b, c) ->  (f a, f b, f c)
  let iter_tuple f = function (a, b, c) -> f a; f b; f c
end

module type Game =
sig
  type state
  type move

  val solved_state : state
  val moves : move list
  val render : state -> unit
  val render_move : move -> unit
  val apply : move -> state -> state
  val legal : state -> move -> bool
end


module Game_Tools (G : Game) :
  (sig
    val randomize : int -> G.state
    val execute : G.move list -> G.state -> G.state
    val pick_random_moves : int -> ?legal:bool -> G.move list
    val legal_moves : G.state -> G.move list
  end) =
struct
  type state = G.state

  let legal_moves state = List.filter G.moves ~f:(G.legal state)
                 
  let execute fs init = List.fold_right ~f:G.apply ~init:init fs

  let rec pick_legal_moves_and_execute acc n state legal =
    if n = 0 then (acc, state)
    else
      let move_set =
        if legal then G.moves
        else legal_moves state
      in
      let random = List.random_element_exn move_set in
      pick_legal_moves_and_execute (random::acc) (n - 1) G.solved_state legal

  let randomize n =
    pick_legal_moves_and_execute [] n G.solved_state true
    |> function (_, x) -> x

  let pick_random_moves n ?legal:(legal = true) =
    pick_legal_moves_and_execute [] n G.solved_state legal
    |> function (moves,  _) -> moves
end



module Eight_Puzzle : Game =
struct
  open Tuple_tools
  type state = (int * int * int) *
               (int * int * int) *
               (int * int * int)

  type transition = state -> state

  type move = Left of transition | Right of transition | Up of transition | Down of transition

  let ( *> ) a b x = x |> a |> b
  let (==) a b = phys_equal a b

  let display_tile x = if x = 0 then "*" else string_of_int x

  let solved_state = ((0, 1, 2), (3, 4, 5), (6, 7, 8))
  let render = iter_tuple (function (a, b, c) -> Printf.printf "%s %s %s\n"
                                                   (display_tile a) (display_tile b) (display_tile c))

  let transpose ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) =
    ((a1, b1, c1), (a2, b2, c2), (a3, b3, c3))

  let push_row_left = function
    | (0, b, c) -> (b, 0, c)
    | (a, 0, c) -> (a, c, 0)
    | x -> x

  let push_row_right = function
    | (a, b, 0) -> (a, 0, b)
    | (a, 0, c) -> (0, a, c)
    | x -> x

  let left = map_tuple push_row_left
  let right = map_tuple push_row_right
  let up = transpose *> left  *> transpose
  let down = transpose *> right *> transpose

  let moves = [Left left; Right right; Up up; Down down]

  let render_move move =
      Printf.printf "%s "
      match move with
      | Left _ -> "left"
      | Right _ -> "right"
      | Up _ -> "up"
      | Down _ ->  "down"

  let _bottom_row  = function (_, _, row) -> row
  let _middle_row  = function (_, row, _) -> row
  let _top_row     = function (row, _, _) -> row
  let _right_col   = transpose *> _bottom_row
  let _middle_col  = transpose *> _middle_row
  let _left_col    = transpose *> _top_row


  let legal state move =
    let has_blank (a, b, c) = (a = 0) || (b = 0) || (c = 0) in
    let multiplex_move = function
    | Left _  -> _right_col
    | Right _ -> _left_col
    | Down _  -> _top_row
    | Up _    -> _bottom_row
    in
    state |> (multiplex_move move) |> has_blank

  let apply f x = match f with
    | Left g -> g x
    | Right g -> g x
    | Up g -> g x
    | Down g -> g x
end



module Search (G : Game) = struct
  type t = G.state
end


module GT = Game_Tools (Eight_Puzzle)

let move = GT.execute (GT.pick_random_moves 1 ~legal:true) Eight_Puzzle.solved_state |> Eight_Puzzle.render

