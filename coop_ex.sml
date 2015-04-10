open Coop

fun foo i () =
  (print ("Starting child thread ["^(Int.toString i)^"]\n");
   print ("Yield ["^(Int.toString i)^"]\n");
   yield ();
   print ("Finishing child thread ["^(Int.toString i)^"]\n");
   i)

fun main () =
  (print "Starting main thread\n";
   fork (foo 1);
   fork (foo 2);
   print "Finishing main thread\n";
   0)

val _ = run main
