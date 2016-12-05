let (|>) f v = f v
module type MT = sig
  type ('a,'b) t
  val write : ('a,'b) t -> 'a -> ('a, [`Write of 'b]) t
  val read  : ('a,'b) t -> 'a * ('a, [`Read of 'b]) t
end

module M : MT = struct
  type ('a,'b) t = 'a ref
  let write r v = r := v; r
  let read r = !r, r
end
