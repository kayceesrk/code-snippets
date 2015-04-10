(** Type to represent effects, where ['a] is the return type of
   the effect. *)
type 'a effect = ..

exception Unhandled

(** [perform e] performs an effect [e].

   @raises Unhandled if there is no active handler. *)
val perform : 'a effect -> 'a

(** Type to represent single-shot continuations waiting for an
   ['a], which will return a [b']. *)
type ('a, 'b) continuation

(** Type to represent effect handlers which return ['b]. *)
type 'b handler =
 { handle: 'a. 'a effect -> ('a, 'b) continuation -> 'b }

(** [handle h f] runs the [f] within an effect handler [h]. *)
val handle: 'b handler -> (unit -> 'b) -> 'b

(** [continue h k x] resumes the continuation [k] within the
   effect handler [h] by passing [x] to [k].

   @raise Invalid_argument if the continuation has already been
   resumed. *)
val continue: 'b handler -> ('a, 'b) continuation -> 'a -> 'b

(** [discontinue h k e] resumes the continuation [k] within the
   effect handler [h] by raising the exception [e] in [k].

   @raise Invalid_argument if the continuation has already been
   resumed. *)
val discontinue: 'b handler -> ('a, 'b) continuation -> exn -> 'b

(** [delegate h e k] is semantically equivalent to:

   {[
     match perform e with
     | v -> continue h k v
     | exception e -> discontinue h k e
   ]}

   but it can be implemented directly more efficiently and is a
   very common case: it is what you should do with effects that
   you don't handle. *)
val delegate: 'b handler -> 'a effect -> ('a, 'b) continuation -> 'b
