#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

long num_iters;

static void work () {
  for (long i = 0; i < num_iters; i++);
}

static void * thread_start(void *arg) {
  int thread_id = (long)arg;
  printf ("thread %d\n", thread_id);
  work ();
  return NULL;
}

int main (int argc, char* argv[]) {
  pthread_t *threads;
  int num_threads;

	if (argc <= 2) {
		fprintf(stderr, "Usage: %s <num_threads> <num_iters>\n", argv[0]);
    exit(1);
	}

  num_threads = atoi(argv[1]);
  num_iters = atoi(argv[2]);

  threads = calloc(num_threads, sizeof(pthread_t));
  if (threads == NULL) {
    fprintf(stderr, "calloc\n");
    exit(1);
  }

  for (long i = 0; i < num_threads - 1; i++) {
    int status = pthread_create(&threads[i], NULL, thread_start, (void*)i);
    if (status != 0) {
      fprintf(stderr, "pthread_create\n");
      exit(1);
    }
  }

  work ();
  for (int i = 0; i < num_threads - 1; i++) {
    pthread_join(threads[i],NULL);
    printf("joined %d\n", i);
  }
  fflush(stdout);

  return 0;
}
