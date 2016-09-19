module type List =
sig
  type ('a,'b) t constraint 'b = [>] * [>]
  val nil  : ('a,'z*'z) t
  val (::) : 'a -> ('a,'b) t -> ('a, [`Succ of 'b]) t
end
