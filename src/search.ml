open Core

module Utils (G : Game.T) = struct
  include Game.Tools(G)
  include Continue_or_stop

  let visited = Map.empty (module G)
  let visited_singleton state = Map.singleton (module G) state (state, None)

  type frontier = (G.t, G.t * G.move option, G.comparator_witness) Map.t

  type graph = (G.t * frontier, frontier) Continue_or_stop.t

  type result = Solved of G.move list * int
              | Ded of int

  let reconstruct solved_state visited =
    let rec reconstruct_path visited state path=
      match Map.find visited state with
      | None | Some (_, None) -> path
      | Some (prev_state, Some last_move) ->
        reconstruct_path visited prev_state (last_move :: path)
    in
    reconstruct_path visited solved_state []

  let handle_tree = function
    | Stop visited                    -> Ded (Map.length visited)
    | Continue (final_state, visited) -> Solved ((reconstruct final_state visited), Map.length visited)

  let not_visited_neighbors state visited =
    let f move =
      let next_state = move <*> state in
      match Map.mem visited next_state with
      | true -> None
      | false -> Some (next_state, Some move)
    in
    legal_moves state
    |> List.filter_map ~f

  let explore_neighbors state visited =
    let f (vis, new_states) move =
      let next_state = move <*> state in
      match Map.add vis ~key:next_state ~data:(state, Some move) with
      | `Duplicate -> (vis, new_states)
      | `Ok v -> (v, next_state :: new_states)
    in
    List.fold ~f ~init:(visited, []) (legal_moves state)
end

(* A basic search space on a gaa*)
module Space (G : Game.T) = struct
  include Utils(G)

  let dfs state =
    let visited = visited_singleton state in

    let rec loop state visited =
      if G.solvedp state then Continue (state, visited)
      else
        let f acc move =
          match acc with
          | Continue (state, visited) -> Continue (state, visited)
          | Stop visited ->
            let next_state = move <*> state in
            match Map.add visited ~key:next_state ~data:(state, Some move) with
            | `Duplicate -> Stop visited
            | `Ok visited -> loop next_state visited
        in
        legal_moves state
        |> List.fold ~f ~init:(Stop visited)
    in
    loop state visited
    |> handle_tree

  let bfs state =
    let visited = visited_singleton state in
    let queue = Queue.singleton state in

    let rec loop queue visited =
      if Queue.is_empty queue then Stop visited
      else
        let state = Queue.dequeue_exn queue in
        if G.solvedp state then Continue (state, visited)
        else
          match explore_neighbors state visited with
          | (visited, new_states) ->
            Queue.enqueue_all queue new_states;
            loop queue visited
    in
    loop queue visited
    |> handle_tree
end

module Metric_space (G : Game.T) (H : Game.H with type t = G.t ) = struct
  include Space(G)
  module Heap = Pairing_heap

  let h = H.heuristic

  let cmp a b = h a - h b

  let best_first state =
    let heap = Heap.create ~cmp () in
    let visited = visited_singleton state in
    Heap.add heap state;

    let rec loop heap visited =
      if Heap.is_empty heap then Stop visited
      else
        let state = Heap.pop_exn heap in
        if G.solvedp state then Continue (state, visited)
        else
          match explore_neighbors state visited with
          | (visited, new_states) ->
            List.iter new_states ~f:(Heap.add heap);
            loop heap visited
    in
    loop heap visited
    |> handle_tree

  let beam k state =
    let visited = visited_singleton state in

    let rec loop k compare visited frontier =
      if List.is_empty frontier
      then Stop visited
      else if List.exists frontier ~f:(G.solvedp)
      then Continue (List.find_exn ~f:(G.solvedp) frontier, visited)
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
          loop k compare visited next_gen
    in
    loop k cmp visited [state]
    |> handle_tree

  let a_star state =
    let cmp (a, g_a) (b, g_b) = (h a + g_a) - (h b + g_b) in
    let heap = Heap.create ~cmp () in
    let visited = visited_singleton state in
    Heap.add heap (state, 0);

    let rec loop heap visited =
      if Heap.is_empty heap then Stop visited
      else
        let state, g = Heap.pop_exn heap in
        if G.solvedp state then Continue (state, visited)
        else
          match explore_neighbors state visited with
          | (visited, new_states) ->
            List.iter new_states ~f:(fun x -> Heap.add heap (x, g + 1));
            loop heap visited
    in
    loop heap visited
    |> handle_tree


  let dijkstra state =
    let cmp (_, c_1, _) (_,c_2, _) = Int.compare c_1 c_2 in
    let heap = Heap.create ~cmp () in
    let visited = visited in
    Heap.add heap (state, 0, (state, None));

    let rec loop frontier visited =
      if Heap.is_empty frontier then Stop visited
      else
        let selected, cost, via = Heap.pop_exn heap in
        let visited = Map.add_exn visited ~key:selected ~data:via in
        if G.solvedp selected then Continue (selected, visited)
        else
          let f (neighbor, via) =
            let weight = 1 in
            let new_cost = cost + weight in
            let new_node = (neighbor, new_cost, via) in
            match Heap.find_elt heap ~f:(fun (a, _,  _) -> G.equal a neighbor) with
            | None -> Heap.add heap new_node
            | Some elt -> Heap.Elt.value_exn elt |> fun (_, prev_cost, _) ->
                if new_cost < prev_cost then
                  ignore (Heap.update heap elt new_node : (G.t * int * (G.t * move option)) Heap.Elt.t);
          in
          not_visited_neighbors selected visited
          |> List.map ~f:(fun (neighbor, move) -> (neighbor, (selected, move)))
          |> List.iter ~f;
          loop heap visited
    in

    loop heap visited
    |> handle_tree
end
