## Biased Ephemerons

Ephemerons biased to domains.

### Structure

    | Header | Domain | Data | Key1 | Key2 | .... | KeyN |

```c
#define Ephe_domain     0
#define Ephe_data       1
#define Ephe_first_key  2
```

### API Calls

Assume `void promote (value* p)` which promotes `*p` to the major heap and
updates `p` to point to the new location.

```c
value _get_key (value e, int i, int is_rpc) {
  domain* me = current_domain ();
  domain* owner = Op_val(e)[Ephe_domain];
  if (owner == me) {
    if (gc_phase == EpheSweep) ephe_clean(e);
    value *kp = &Op_val(e)[i + Ephe_first_key];
    if (is_rpc) promote(kp);
    return *kp;
  } else {
    return rpc (owner, fun () -> _get_key(e, i, 1));
  }
}

value get_key (value e, int i) {
  return _get_key(e,i,0);
}

value set_key (value e, int i, value k) {
  domain* me = current_domain ();
  domain* owner = Op_val(e)[Ephe_domain];
  if (owner = me) {
    if (gc_phase == EpheSweep) ephe_clean(e);
    Op_val(e)[i + Ephe_first_key] = k;
  } else {
    promote (&k);
    rpc (owner, fun () -> set_key(e,i,k));
  }
  return_unit;
}

value _get_data (value e, int is_rpc) {
  domain* me = current_domain ();
  domain* owner = Op_val(e)[Ephe_domain];
  if (owner == me) {
    if (gc_phase == EpheSweep) ephe_clean(e);
    value* dp = &Op_val(e)[Ephe_data];
    if (is_rpc) promote(dp);
    return *dp;
  } else {
    return rpc (owner, fun () -> _get_data(e, 1));
  }
}

value get_data (value e) {
  return _get_data(e, 0);
}

value set_data (value e, value d) {
  domain* me = current_domain ();
  domain* owner = Op_val(e)[Ephe_domain];
  if (owner = me) {
    if (gc_phase == EpheSweep) ephe_clean(e);
    Op_val(e)[Ephe_data] = d;
  } else {
    d = promote (d);
    rpc (owner, fun () -> set_data(e,d));
  }
  return_unit;
}
```

### GC

In this above, an ephemeron owned by a domain will never its keys or data in
foreign minor heaps. Hence, a domain's `ephe_ref_table` only contains references
to ephemerons that is owned by that domain into its own minor heap. Hence, minor
GC does not need to synchronise with other domains for ephemeron cleaning.
Moreover, all API calls are routed to the owner. Hence, the GC does not have to
worry about concurrent updates to ephemerons.
