signature COOP =
sig
  val run  : (unit -> int) -> int
  val fork : (unit -> int) -> unit
  val yield : unit -> unit
end

structure Coop :> COOP =
struct
  exception Fork of (unit -> int)
  exception Yield

  structure E = Eff (struct
                        type cont_arg = unit
                        type cont_res = int  (* status *)
                    end)

  open E

  fun fork f = perform (Fork f)
  fun yield () = perform (Yield)

  fun run main =
    let
      val threads = ref []
      fun enqueue t = threads := !threads @ [t]
      fun dequeue () =
        case !threads of
          [] => 0
        | k :: ks =>
            (threads := ks;
             print ("run.dequeue: thread finished with "^(Int.toString (continue scheduler k ()))^"\n");
             dequeue ())
      and scheduler eff cont =
        case eff of
          Yield =>
            (enqueue cont;
             dequeue ())
        | Fork f =>
            (enqueue cont;
             print ("run.Fork: thread finished with "^(Int.toString (handleFun scheduler f))^"\n");
             dequeue ())
        | _ => raise Fail "scheduler: unhandled effect"
    in
      handleFun scheduler main
    end
end
