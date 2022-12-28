module type EqType = sig
  type t
  val eq : t -> t -> bool
end

module type S = sig
  type key
  type 'a t
  val make : int -> 'a t
  val lookup : 'a t -> key -> 'a option
  val insert : 'a t -> key -> 'a -> [`Already_set of 'a | `Full | `Success ]
end

module Make (Eq: EqType) : S with type key = Eq.t = struct

  type key = Eq.t

  type 'a element = Unset | Set of {key : key; value : 'a}

  type 'a t = 'a element Atomic.t array

  let make size = Array.init size (fun _ -> Atomic.make Unset)

  let hash arr key = Hashtbl.hash key mod Array.length arr

  type 'a get_entry_result =
    | Full (* Hash table is full *)
    | No_key of 'a element Atomic.t (* Atomic ref is the slot *)
    | Value of 'a

  let get_entry arr key =
    let index = hash arr key in
    let arr_len = Array.length arr in
    let rec probe index tries =
      if tries = arr_len then Full
      else begin
        let r = arr.(index) in
        match Atomic.get r with
        | Unset -> No_key r
        | Set {key = key'; value} ->
            if key' = key then Value value
            else probe (index + 1 mod arr_len) (tries + 1)
      end
    in
    probe index 0

  let lookup arr key =
    match get_entry arr key with
    | Full | No_key _ -> None
    | Value v -> Some v

  let insert arr key value =
    let e = Set {key; value} in
    let rec loop () =
      match get_entry arr key with
      | Full -> `Full
      | No_key r ->
          if Atomic.compare_and_set r Unset e then `Success
          else loop ()
      | Value v -> `Already_set v
    in
    loop ()
end
