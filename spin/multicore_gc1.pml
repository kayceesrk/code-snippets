#define N 2

byte num_domains_to_mark;
byte num_domains_to_sweep;

proctype major_slice () {
  bool sweepDone = false;
	bool markDone = false;

  (!sweepDone) ->
	  //Do sweep
	  atomic {
  	  num_domains_to_sweep--;
		}
		sweepDone = true;

  (!markDone) ->
	  //Do mark
	  atomic {
		  num_domains_to_mark--;
		}
		markDone = true;

  assert (sweepDone && markDone);
}

init {
  byte num_proc;

  num_domains_to_mark = N;
	num_domains_to_sweep = N;

  atomic {
	  num_proc = 0;
		do
		:: num_proc < N ->
		    run major_slice();
				num_proc++;
		:: num_proc == N ->
		    break;
		od
	}
}
