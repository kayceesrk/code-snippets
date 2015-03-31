fun 'a f () =
let
  exception E of 'a
in
  (E,
   fn v => case v of
       E x => SOME x
     | _ => NONE)
end
