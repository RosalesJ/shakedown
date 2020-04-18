open Core

module Space (G : Game.T) = struct
  module GT = Game.Tools(G)

  let visited = Map.empty (module G)
  let visited_singleton state = Map.singleton (module G) state (state, None)

  type tree = (G.t, G.t * G.move option, G.comparator_witness) Map.t

  type t = Solved of G.t * tree
         | Deadend of tree

  let reconstruct solved_state visited =
    let rec reconstruct_path visited state path=
      match Map.find visited state with
      | None | Some (_, None) -> path
      | Some (prev_state, Some last_move) ->
        reconstruct_path visited prev_state (last_move :: path)
    in
    reconstruct_path visited solved_state []

  let handle_tree = function
    | Deadend visited -> None, Map.length visited
    | Solved (solved, visited) -> Some (reconstruct solved visited), Map.length visited

  let not_visited_neighbors state visited =
    let f move =
      let next_state = G.apply state move in
      match Map.mem visited next_state with
      | true -> None
      | false -> Some (next_state, Some move)
    in
    GT.legal_moves state
    |> List.filter_map ~f

  let explore_neighbors state visited =
    let f (vis, new_states) move =
      let next_state = G.apply state move in
      match Map.add vis ~key:next_state ~data:(state, Some move) with
      | `Duplicate -> (vis, new_states)
      | `Ok v -> (v, next_state :: new_states)
    in
    List.fold ~f ~init:(visited, []) (GT.legal_moves state)

  let rec dfs_rec state visited =
    if G.solved state then Solved (state, visited)
    else
      let f acc move =
        match acc with
        | Solved (state, visited) -> Solved (state, visited)
        | Deadend visited ->
          let next_state = G.apply state move in
          match Map.add visited ~key:next_state ~data:(state, Some move) with
          | `Duplicate -> Deadend visited
          | `Ok visited -> dfs_rec next_state visited
      in
      GT.legal_moves state
      |> List.fold ~f ~init:(Deadend visited)

  let dfs state =
    let visited = Map.add_exn ~key:state ~data:(state, None) visited in
    dfs_rec state visited
    |> handle_tree


  let rec bfs_rec queue visited =
    if Queue.is_empty queue then Deadend visited
    else
      let state = Queue.dequeue_exn queue in
      if G.solved state then Solved (state, visited)
      else
        match explore_neighbors state visited with
        | (visited, new_states) ->
          Queue.enqueue_all queue new_states;
          bfs_rec queue visited

  let bfs state =
    let visited = visited_singleton state in
    let queue = Queue.singleton state in
    bfs_rec queue visited
    |> handle_tree

  let rec best_first_rec heap visited =
    if Heap.is_empty heap then Deadend visited
    else
      let state = Heap.pop_exn heap in
      if G.solved state then Solved (state, visited)
      else
        match explore_neighbors state visited with
        | (visited, new_states) ->
          List.iter new_states ~f:(Heap.add heap);
          best_first_rec heap visited
end

module Metric_space (G : Game.T) (H : Game.H with type h = G.t ) = struct
  include Space(G)

  let h = H.heuristic

  let best_first state =
    let cmp a b = h a - h b in
    let heap = Heap.create ~cmp () in
    let visited = visited_singleton state in
    Heap.add heap state;

    best_first_rec heap visited
    |> handle_tree

  let rec beam_rec k compare visited frontier=
    if List.is_empty frontier
    then Deadend visited
    else if List.exists frontier ~f:(G.solved)
    then Solved (List.find_exn ~f:(G.solved) frontier, visited)
    else
      let f (visited, states) x =
      match explore_neighbors x visited with
        | (visited, new_states) ->
          (visited, List.rev_append new_states states)
      in
      List.fold frontier ~f ~init:(visited, [])
      |> function (visited, states) ->
        let sorted = List.sort ~compare states in
        let next_gen = List.take sorted k in
        beam_rec k compare visited next_gen

  let beam k state =
    let compare s t = h s - h t in
    let visited = visited_singleton state in
    beam_rec k compare visited [state]
    |> handle_tree

  let rec a_star_rec heap visited =
    if Heap.is_empty heap then Deadend visited
    else
      let state, g = Heap.pop_exn heap in
      if G.solved state then Solved (state, visited)
      else
        match explore_neighbors state visited with
        | (visited, new_states) ->
          List.iter new_states ~f:(fun x -> Heap.add heap (x, g + 1));
          a_star_rec heap visited

  let a_star state =
    let cmp (a, g_a) (b, g_b) = (h a + g_a) - (h b + g_b) in
    let heap = Heap.create ~cmp () in
    let visited = visited_singleton state in
    Heap.add heap (state, 0);
    a_star_rec heap visited
    |> handle_tree
end
