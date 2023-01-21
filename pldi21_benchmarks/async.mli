type 'a promise
val async : 'a Sched_monad.t -> 'a promise Sched_monad.t
val await : 'a promise -> 'a Sched_monad.t
