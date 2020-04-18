open Core

module type T =
sig
  include Comparator.S
  type move

  val solved : t -> bool
  val solved_witness : t
  val moves : move list
  val render : t -> unit
  val render_move : move -> unit
  val apply : t -> move -> t
  val legal : t -> move -> bool
end

(* I need to try and get this to be bette *)
module type H =
sig
  type h
  val heuristic : h -> int
end

module type R =
sig
  include T
  val heuristic : t -> int
end

module With_Heuristic = functor (G : T) (Heu : H with type h = G.t) ->
struct
  include G
  include Heu
end

module Tools (G : T) :
  (sig
    val random_state : G.t -> int -> G.t
    val execute : G.move list -> G.t -> G.t
    val pick_random_moves : G.t -> int -> bool -> G.move list
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

  let random_state state n =
    pick_legal_moves_and_execute [] n state true
    |> function (_, x) -> x

  let pick_random_moves state n legal =
    pick_legal_moves_and_execute [] n state legal
    |> function (moves,  _) -> moves
end
