open Core
open Game

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

  let handle_tree = function 
    | Deadend visited -> None, Map.length visited
    | Solved visited -> Some (reconstruct visited), Map.length visited

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
    dfs_rec state visited
    |> handle_tree

  
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
    bfs_rec queue visited
    |> handle_tree

end
