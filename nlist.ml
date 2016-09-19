type 'a s

module NList :
sig
  type (+'length, +'elem_type) t
  val nil : ('z*'z, 'a) t
  val cons : 'a * ('m*'n, 'a) t -> ('m*'n s, 'a) t
  val append : ('m*'n, 'a) t * ('l*'m, 'a) t -> ('l*'n, 'a) t
  val to_list : ('i, 'a) t -> 'a list
end
=
struct
  type ('i, 'a) t = 'a list
  let nil = []
  let cons (x, xs) = x :: xs
  let append (xs, ys) = xs @ ys
  let to_list xs = xs
end
