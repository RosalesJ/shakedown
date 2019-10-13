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
  let map_tuple f (a, b, c) = (f a, f b, f c)
  let iter_tuple f (a, b, c) = f a; f b; f c
  let fold_tuple f (a, b, c) = (f (f a b) c)
  let hash (a, b, c) = a + 13 * (b * 13 + c)
end

module type Game =
sig
  include Comparator.S
  type move
  
  val solved_state : t
  val moves : move list
  val render : t -> unit
  val render_move : move -> unit
  val apply : t -> move -> t
  val legal : t -> move -> bool
end

module Game_Tools (G : Game) :
  (sig
    val randomize_stat : int -> G.t
    val execute : G.move list -> G.t -> G.t
    val pick_random_moves : int -> ?legal:bool -> G.move list
    val legal_moves : G.t -> G.move list
    val render_moves : G.move list -> unit
  end) =
struct
  include G
  
  let render_moves moves =
    Printf.printf "[";
    List.iter ~f:render_move moves;
    Printf.printf "]\n"

  let legal_moves state = List.filter moves ~f:(legal state)
                 
  let execute fs init = List.fold ~f:apply ~init:init fs

  let rec pick_legal_moves_and_execute acc n state legal =
    if n = 0 then (List.rev acc, state)
    else
      let move_set =
        if legal then legal_moves state
        else moves
      in
      let random = List.random_element_exn move_set in
      let new_state = apply state random in
      pick_legal_moves_and_execute (random::acc) (n - 1) new_state legal

  let randomize_stat n =
    pick_legal_moves_and_execute [] n solved_state true
    |> function (_, x) -> x

  let pick_random_moves n ?legal:(legal = true) =
    pick_legal_moves_and_execute [] n solved_state legal
    |> function (moves,  _) -> moves
end



module Eight_Puzzle : Game =
struct
  include Tuple_tools
  module T = struct
    type t = (int * int * int) *
             (int * int * int) *
             (int * int * int)  [@@deriving sexp]
    let f a b =  a + 1693 * b
    let hash_state x = fold_tuple f (map_tuple hash x)

    let compare a b = (hash_state a) - (hash_state b)
  end

  include T
  include Comparator.Make(T)

  type transition = t -> t

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

module Search (G : Game) = struct
  module Tools = Game_Tools(G)
  
  let visited = Set.empty (module G)

  let member = Set.mem visited G.solved_state

  type t = Solved of G.move list
         | DeadEnd of (G.t, G.comparator_witness) Set.t

  let dfs state visited path =
    match G.solved_state = state with
    | true -> Visited path
    | false ->
      let f acc move = match acc with
        | Solved x -> x
        | Deadend visited -> dfs (G.apply state move) visited (move :: path)
      in
      
end
