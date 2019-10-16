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

module Tuple_Tools = struct
  let map_tuple f (a, b, c) = (f a, f b, f c)
  let iter_tuple f (a, b, c) = f a; f b; f c
  let fold_tuple f (a, b, c) = (f (f a b) c)
  let hash const (a, b, c) = a + const * (b + (const * c))
  let render (a, b, c) = Printf.printf "(%i, %i, %i)\n" a b c
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
    val random_state : int -> G.t
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

  let random_state n =
    pick_legal_moves_and_execute [] n solved_state true
    |> function (_, x) -> x

  let pick_random_moves n ?legal:(legal = true) =
    pick_legal_moves_and_execute [] n solved_state legal
    |> function (moves,  _) -> moves
end



module Eight_Puzzle : Game =
struct
  include Tuple_Tools
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

module Eight_Moves = struct
  let moves = Eight_Puzzle.moves

  let left = List.nth_exn moves 0
  let right = List.nth_exn moves 1
  let up = List.nth_exn moves 2
  let down = List.nth_exn moves 3
end

module Search (G : Game) = struct
  module Tools = Game_Tools(G)

  let visited = Map.empty (module G)

  let member = Map.mem visited G.solved_state

  type tree = (G.t, G.t * G.move option, G.comparator_witness) Map.t

  type t = Solved of tree
         | Deadend of tree

  let reconstruct visited =
    let rec reconstruct_path visited state path=
      match Map.find visited state with
      | None | Some (_, None) -> path
      | Some (prev_state, Some last_move) ->
        reconstruct_path visited prev_state (last_move :: path)
    in
    reconstruct_path visited G.solved_state []

  let not_visited_neighbors state visited =
    let f move =
      let next_state = G.apply state move in
      match Map.mem visited next_state with
      | true -> None
      | false -> Some (next_state, Some move)
    in
    Tools.legal_moves state
    |> List.filter_map ~f

  let explore_neighbors state visited =
    let f (vis, new_states) move =
      let next_state = G.apply state move in
      match Map.add vis ~key:next_state ~data:(state, Some move) with
      | `Duplicate -> (vis, new_states)
      | `Ok v -> (v, next_state :: new_states)
    in
    List.fold ~f ~init:(visited, []) (Tools.legal_moves state)

  let rec dfs_rec state visited =
    if G.solved_state = state then Solved visited
    else
      let f acc move =
        match acc with
        | Solved visited -> Solved visited
        | Deadend visited ->
          let next_state = G.apply state move in
          match Map.add visited ~key:next_state ~data:(state, Some move) with
          | `Duplicate -> Deadend visited
          | `Ok visited -> dfs_rec next_state visited
      in
      Tools.legal_moves state
      |> List.fold ~f ~init:(Deadend visited)

  let dfs state =
    let visited = Map.add_exn ~key:state ~data:(state, None) visited in
    match dfs_rec state visited with
    | Deadend _ -> None
    | Solved visited -> Some (reconstruct visited)

  
  let rec bfs_rec queue visited =
    if Queue.is_empty queue then Deadend visited
    else
      let state = Queue.dequeue_exn queue in
      if G.solved_state = state then Solved visited
      else 
        match explore_neighbors state visited with
        | (visited, new_states) ->
          Queue.enqueue_all queue new_states;
          bfs_rec queue visited

  let bfs state =
    let visited = Map.singleton (module G) state (state, None) in
    let queue = Queue.singleton state in
    match bfs_rec queue visited with
    | Deadend visited -> None, Map.length visited
    | Solved visited -> Some (reconstruct visited), Map.length visited
end


let () =
  let module EP = Search(Eight_Puzzle) in
  let module ET = Game_Tools(Eight_Puzzle) in
  let open Eight_Moves in

  let random_state = ET.random_state 1000 in

  Printf.printf "%s\n" "Original state:";
  Eight_Puzzle.render random_state;

  EP.bfs random_state
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
  let open Eight_Moves in
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
