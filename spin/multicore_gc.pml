#define N 2

byte num_domains_to_mark;
byte num_domains_to_sweep;
byte num_domains_to_ephe_sweep;
int ephe_cycle;
int ephe_cycle_in_domain[N];
bool ephe_sweep;


proctype major_slice (byte my_domain_id) {
  bool sweep_done = false;
  bool mark_done = false;
  int saved_ephe_cycle;
  bool ephe_sweep_done = false;

  //To model that there is a fixed amount of marking to do.
  int mark_work_left = 256;
  byte i;
  bool done = false;

  again:

  assert (num_domains_to_mark >= 0);
  assert (num_domains_to_sweep >= 0);
  assert (num_domains_to_ephe_sweep >= 0);

  if
  :: !sweep_done -> {
      //Do sweep
      atomic { num_domains_to_sweep--; };
      sweep_done = true
     }
  :: !mark_done -> {
      //Do mark
      if
      :: num_domains_to_mark == 1 -> atomic { ephe_cycle++ };
      :: else
      fi;
      atomic { num_domains_to_mark--; };
      mark_done = true;
     }
  :: num_domains_to_sweep == 0 &&
     num_domains_to_mark == 0 -> {
       if
       :: ephe_cycle > ephe_cycle_in_domain[my_domain_id] ->
             saved_ephe_cycle = ephe_cycle;
             if //epheMark
             :: mark_work_left > 0 ->
                 atomic { num_domains_to_mark++; };
                 mark_done = false;
                 mark_work_left--;
                 printf ("mark_work_left=%d\n", mark_work_left);
                 goto again
             :: else
             fi;
             if
             :: mark_done ->
                 ephe_cycle_in_domain[my_domain_id] = saved_ephe_cycle;
             :: else
             fi
       :: else
       fi
     }
  :: num_domains_to_sweep == 0 &&
     num_domains_to_mark == 0 &&
     !ephe_sweep -> {
        saved_ephe_cycle = ephe_cycle;
        i = 0;
        do
        :: i < N ->
              if
              :: saved_ephe_cycle != ephe_cycle_in_domain[i] -> break
              :: else
              fi
        :: i == N -> break
        od
        if
        :: i == N && saved_ephe_cycle == ephe_cycle ->
              ephe_sweep = true
        :: else
        fi;
     }
  :: ephe_sweep -> {
    if
    :: !ephe_sweep_done ->
          ephe_sweep_done = 1;
          atomic { num_domains_to_ephe_sweep--; }
    :: else
    fi;
    if
    :: num_domains_to_ephe_sweep == 0 ->
         progress: done = true
    :: else -> goto again
    fi
  }
  fi
}

init {
  byte i = 0;

  num_domains_to_mark = N;
  num_domains_to_sweep = N;
  num_domains_to_ephe_sweep = N;
  ephe_cycle = 0;
  do
  :: i < N ->
       ephe_cycle_in_domain[i] = 0;
       i++;
  :: i == N -> break
  od;
  ephe_sweep = false;

  atomic {
    i = 0;
    do
    :: i < N ->
        run major_slice(i);
        i++;
    :: i == N ->
        break;
    od
  }
}
