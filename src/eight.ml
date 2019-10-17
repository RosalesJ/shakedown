open Core
open Game


module Eight_Puzzle : Game =
struct
  include Common.Tuple_Tools
  module T = struct
    type t = (int * int * int) *
             (int * int * int) *
             (int * int * int)  [@@deriving sexp]
    let hash_state x = hash 1693 (map_tuple (hash 13) x)

    let compare a b = (hash_state a) - (hash_state b)
  end

  include T
  include Comparator.Make(T)

  type transition = t -> t

  type move = Left of transition | Right of transition | Up of transition | Down of transition

  let ( *> ) a b x = x |> a |> b
  (* let (==) a b = phys_equal a b *)

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

  let right = map_tuple push_row_left
  let left = map_tuple push_row_right
  let up = transpose *> left  *> transpose
  let down = transpose *> right *> transpose

  let moves = [Left left; Right right; Up up; Down down]

  let render_move move =
      Printf.printf "%s "
      (match move with
       | Left _ -> "left"
       | Right _ -> "right"
       | Up _ -> "up"
       | Down _ ->  "down")

  let _bottom_row  = function (_, _, row) -> row
  let _middle_row  = function (_, row, _) -> row
  let _top_row     = function (row, _, _) -> row
  let _right_col   = transpose *> _bottom_row
  let _middle_col  = transpose *> _middle_row
  let _left_col    = transpose *> _top_row


  let legal state move =
    let has_no_blank (a, b, c) = not ((a = 0) || (b = 0) || (c = 0)) in
    let multiplex_move = function
    | Left _  -> _left_col
    | Right _ -> _right_col
    | Down _  -> _bottom_row
    | Up _    -> _top_row
    in
    state |> (multiplex_move move) |> has_no_blank

  let apply x f = match f with
    | Left g -> g x
    | Right g -> g x
    | Up g -> g x
    | Down g -> g x
end

module Eight_Moves = struct
  let moves = Eight_Puzzle.moves

  let left = List.nth_exn moves 0
  let right = List.nth_exn moves 1
  let up = List.nth_exn moves 2
  let down = List.nth_exn moves 3
end
