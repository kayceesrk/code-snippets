signature EFF =
sig
  type cont_arg    (* Type of result of effects *)
  type cont_res   (* Type of result of continuations *)

  type eff = exn  (* Effect type *)
  type cont       (* Continuation type *)

  type handler = eff -> cont -> cont_res (* handler type *)

  val perform     : eff -> cont_arg
  val handleFun   : handler -> (unit -> cont_res) -> cont_res
  val continue    : handler -> cont -> cont_arg -> cont_res
  val discontinue : handler -> cont -> exn -> cont_res
end

functor Eff (S: sig
                  type cont_arg
                  type cont_res
                end) : EFF =
struct
  structure MT = MLton.Thread

  type cont_arg = S.cont_arg
  type cont_res = S.cont_res

  type eff = exn

  datatype arg =
    ArgVal of cont_arg
  | Exception of exn

  datatype result =
    ResVal of cont_res
  | Effect of (eff * arg MT.t)

  type cont = arg MT.t
  type handler = eff -> cont -> cont_res

  val parentContRef : result MT.t option ref = ref NONE

  fun perform e =
    case !parentContRef of
      NONE => raise Fail "No parent thread"
    | SOME pt =>
        let
          val a = MT.switch (fn ct => MT.prepare (pt, Effect (e, ct)))
        in
          case a of
            ArgVal v => v
          | Exception e => raise e
        end

  fun continueCore handler rt =
  let
    val pt = !parentContRef
    val r = MT.switch (fn t => (parentContRef := SOME t; rt))
    val () = parentContRef := pt
  in
    case r of
      ResVal v => v
    | Effect (e, ct) => handler e ct
  end

  fun handleFun handler f =
  let
    fun mkThrd f () =
    let
      val res = f ()
    in
      case !parentContRef of
        NONE => raise Fail "No parent thread"
      | SOME pt => MT.switch (fn ct => MT.prepare (pt, ResVal res))
    end
    val f' = mkThrd f
    val newRT = MT.prepare (MT.new f', ())
  in
    continueCore handler newRT
  end

  fun continue handler cont v =
    continueCore handler (MT.prepare (cont, ArgVal v))

  fun discontinue handler cont e =
    continueCore handler (MT.prepare (cont, Exception e))
end
