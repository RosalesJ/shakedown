open Core

module Knight = struct
  module T = struct
    type t = int * int [@@deriving sexp]

    let legal_moves ((x, y) : t) =
      [ (x + 2, y + 1) ; (x + 2, y - 1)
      ; (x + 1, y + 2) ; (x + 1, y - 2)
      ; (x - 2, y + 1) ; (x - 2, y - 1)
      ; (x - 1, y + 2) ; (x - 1, y - 2)]

    let spiral_label ((x, y) : t) =
      let n = max (abs x) (abs y) in
      let s = (2 * n - 1) * (2 * n - 1) in
                                                         
      if      n = (abs y) && y > 0 then   x  + s + 7 * n  (* top wall *)
      else if n = (abs x) && x < 0 then   y  + s + 5 * n  (* left wall *)
      else if n = (abs y) && y < 0 then (-x) + s + 3 * n  (* bottom wall *)
      else if n = (abs x) && x > 0 then (-y) + s + n      (* right wall *)
      else 1

    let compare a b = compare (spiral_label a) (spiral_label b)
  end

  include T
  include Comparator.Make(T)
end

let render_board r visited final =
  let f i pos =
    let newline = if (i + 1) mod (2 * r) = 0 then "\n" else "" in
    (if pos = final then
      "><"
    else if Set.mem visited pos then
      "██"
    else
      "╌╌") ^ newline
  in
  let ran = Sequence.range (-r) r in
  Sequence.cartesian_product ran ran
  |> Sequence.mapi ~f
  |> Sequence.to_array
  |> String.concat_array
  |> Printf.printf "%s\n"

let () =
  let rec loop pos visited path =
    let visited = Set.add visited pos in
    match
      Knight.legal_moves pos
      |> List.filter ~f:(fun x -> not (Set.mem visited x))
      |> List.min_elt ~compare:Knight.compare
    with
    | None -> (visited, List.rev path)
    | Some p -> loop p visited (p :: path)
  in
  
  match loop (0, 0) (Set.empty (module Knight)) [(0, 0)] with
  | (visited, path) -> render_board 30 visited (List.last_exn path);
    Printf.printf "Total visited: %d\n" (List.length path)
          
