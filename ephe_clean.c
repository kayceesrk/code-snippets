/* Assume that the ephemeron is only composed of a single key and data.
 * Mutator can concurrently modify the keys and data.
 *
 * TODO: Considers only minor GCs. Expand to cover major GC. */

typedef struct {
  value key;
  value data;
} ephemeron_t;

typedef struct {
  value* key;
  value old_key;
  value* data;
  value old_data;
} ephe_clean_descriptor_t;

/* Clean the data for dead key. Has a single linearlization point. */
void ephe_clean (ephe_clean_descriptor_t *d) {
  assert (Is_dead(d->old_key));
  d->old_data = *(d->data);
  /* d->data might have been updated by the mnutator in the mean time. As long
   * as the key remains the old_key, the new data will be released. */
  if (!__sync_bool_compare_and_swap (d->data, d->old_data, d)) { //Linearization point.
    /* data updated. try again to avoid losing writes to data. */
    return ephe_clean(d);
  }
  k = *(d->key);
  if (k == d->old_key) {
    if (!__sync_bool_compare_and_swap (d->data, d, None_val)) {
      /* data updated. try again to avoid losing writes to data. */
      return ephe_clean(d);
    }
  } else {
    /* Key has been updated. Clean should be a noop. Try to change the data
     * back to original one. OK for CAS to fail since we need to avoid losing
     * writes to data. */
    __sync_bool_compare_and_swap (d->data, d, d->old_data);
  }
}

int set_key (ephemeron_t* e, value k) {
  e->key = k;
}

int set_data (ephemeron_t* e, value d) {
  e->data = d;
}

int get_key (ephemeron_t* e) {
  if (Is_foreign(e->key)) {
    read_fault(e->key);
  }
  return e->key;
}

int get_data (ephemeron_t* e) {
  d = e->data;
  if (Is_descriptor(d)) {
    ephe_clean(d);
    return get_data(e);
  }
  return d;
}
