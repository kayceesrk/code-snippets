#  IntegriDB — Authenticated Interval Tree in OCaml

This repository provides a toy implementation of **IntegriDB**, a simplified cryptographic authenticated data structure using an **Interval Tree** for range queries with cryptographic integrity guarantees.

It is implemented entirely in **OCaml** using the [`digestif`](https://github.com/mirage/digestif) library for secure hash functions (BLAKE2b).

---

##  Features

-  Defines a generic authenticated interval tree (AIT)
-  Supports range queries over ordered keys (e.g., salary)
-  Verifies result integrity by recomputing and comparing root hash
-  Written in functional OCaml with recursive tree logic

---

##  How It Works

1. **Data Insertion**: Tree is built from a list of sorted key-value pairs. Each leaf hashes its data; each internal node hashes its children's hashes.

2. **Query**: A range query (`lo, hi`) returns the data matching that range, along with **cryptographic proof** (partial hashes used to compute the root).

3. **Verification**: The query recomputes the root hash from the result and the proof path. If the recomputed root matches the actual root hash — verified.

---

## Example

Given this dataset:

```ocaml
let employee_data = [
  (50000, "Alice");
  (80000, "Bob");
  (120000, "Charlie");
  (150000, "Daisy");
  (200000, "Ethan");
  (250000, "Fiona");
]
