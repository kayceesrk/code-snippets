
(* Quicksort using the parallel scheduler *)

let swap a b t =
  let t_b = t.(b) in
  t.(b) <- t.(a);
  t.(a) <- t_b

let quicksort t =
  let split start length pivot_pos =
    let pivot = t.(pivot_pos ) in
    swap start pivot_pos t;
    let low , high = ref (start + 1) , ref (start + length - 1) in
    while !low < ! high do
      while !low < ! high && t.(! low) <= pivot do
        incr low
      done ;
      while !low < ! high && t.(! high ) >= pivot do
        decr high
      done ;
      if !low < ! high then swap ! low ! high t
    done ;
    if t .(! low) > pivot then decr low;
    swap start ! low t;
    !low;
  in
  let rec sort start length () =
    if length > 1 then begin
      let pivot_pos = start + Random .int length in
      let new_pos = split start length pivot_pos in
        if length > 200 then begin
          Sched_multi.fork (sort start (new_pos - start ));
          Sched_multi.fork (sort (new_pos + 1) (start + length - new_pos - 1));
      end else begin
          sort start (new_pos - start ) ();
          sort (new_pos + 1) (start + length - new_pos - 1) ();
      end
    end
  in
    sort 0 (Array . length t)

let arr = Array.init 2000000 (fun _ -> Random.int 2000000)

let () = Sched_multi.run (quicksort arr)

let () =
  for i = 1 to 10 do
    print_endline (string_of_int arr.(i))
  done
