module Tuple_tools = struct
  let map_tuple f (a, b, c) = (f a, f b, f c)
  let iter_tuple f (a, b, c) = f a; f b; f c
  let fold_tuple f (a, b, c) = (f (f a b) c)
  let hash const (a, b, c) = a + const * (b + (const * c))
  let render (a, b, c) = Printf.printf "(%i, %i, %i)\n" a b c
end
