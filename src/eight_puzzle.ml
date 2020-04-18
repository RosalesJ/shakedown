open Core
open Common

include Tuple_tools

module T = struct
  type row = int * int * int [@@deriving sexp]
  type t   = row * row * row [@@deriving sexp]
  let hash_state x = hash 1693 (map_tuple (hash 13) x)

  let compare a b = (hash_state a) - (hash_state b)
end

include T
include Comparator.Make(T)

type move = Left of  (t -> t)
          | Right of (t -> t)
          | Up of    (t -> t)
          | Down of  (t -> t)

let ( *> ) a b x = x |> a |> b
(* let (==) a b = phys_equal a b *)

let display_tile x = if x = 0 then "*" else string_of_int x

let solved_witness = ((0, 1, 2), (3, 4, 5), (6, 7, 8))

let solvedp = (=) solved_witness

let render = iter_tuple (function (a, b, c) -> Printf.printf "%s %s %s\n"
                                                 (display_tile a) (display_tile b) (display_tile c))

let transpose : t -> t = function  ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) ->
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

let apply x = function
  | Left g -> g x
  | Right g -> g x
  | Up g -> g x
  | Down g -> g x

let flatten ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) = [a1; a2; a3; b1; b2; b3; c1; c2; c3]
let flat_solved = flatten solved_witness

(* Count how many tiles are out of place *)
module H1 =
struct
  include T

  let heuristic state =
    let f acc (x, y) =
      match x = y with
      | true -> acc
      | false -> acc + 1
    in
    flatten state
    |> List.zip_exn flat_solved
    |> List.fold ~f ~init:0
end

(* The taxicab distance between the state and the goal *)
module H2 =
struct
  include T

  let location num (x, y, z) =
    let num_in (a, b, c) =
      if num = a then Some 0
      else if num = b then Some 1
      else if num = c then Some 2
      else None
    in
    match num_in x with
    | Some y -> (0, y)
    | None ->
      match num_in y with
      | Some y -> (1, y)
      | None ->
        match num_in z with
        | Some y -> (2, y)
        | None -> (-1, -1)

  let heuristic state =
    let taxicab (x1, y1) (x2, y2) =
      abs (x1 - x2) + abs (y1 - y2)
    in
    let compare num =
      let home = location num solved_witness in
      let rand = location num state in
      taxicab home rand
    in
    let f acc x =
      acc + compare x
    in
    List.fold ~f flat_solved ~init:0
end
