#include "stdlib.h"
#define FRAY_STATS
#include "fray.h"
#include "stdio.h"

/********* TEST ************/
/* measure rate at which random but dependent locations can be loaded */
int foop;
#define PRIME 262584211
#define TABLE_SIZE (1<<25)
#define NUM_SWAPS 10000000
uint64_t a[TABLE_SIZE];
uint64_t perm[TABLE_SIZE];
void chase(fray_block *fb, int64_t id) {
  int iter = NUM_SWAPS/fb->sched;
  uint64_t s = uint64_t(a+id);

  while(iter--) {
    //    fprintf(stderr, "chase: fb %p id %lld p %p\n", fb, (long long int) id, p);
    fray_prefetch_and_yield(fb, (void*) s);
    s = *((uint64_t*)s);
  }
  foop = s;
  fray_return(fb);
}
void goof(fray_block *fb, int64_t id) {
  int i;
  fprintf(stderr, "goof %ld\n", id);
  for (i = 0; i < id; i++) {
    fray_yield(fb);
  }
  fprintf(stderr, "%ld arrived at barrier 1\n", id);
  fray_barrier(fb);
  fprintf(stderr, "%ld freed from barrier 1\n", id);
  if (random() & 0x1) {
    fprintf(stderr, "%ld arrived at barrier 2\n", id);
    fray_barrier(fb);
    fprintf(stderr, "%ld left from barrier 2\n", id);
  }
  else {
    fprintf(stderr, "%ld skipping barrier 2\n", id);
  }
  fray_return(fb);
}
void hoof(fray_block *fb, int64_t id) {
  if (id) {
    fray_barrier(fb);
    fprintf(stderr, "%ld returned from barrier 3\n", id);
    fray_return(fb);
  }
  else fray_return(fb);
}
void poof(fray_block *fb, int64_t id) {
  int i, r;
  for (i = 0, r = random()&3; i <= r; i++) {
    fprintf(stderr, "%d: Here's %d!\n", i, (int) id+1);
    fray_yield(fb);
  }
  fray_return(fb);
}

#include <sys/time.h>
//#include <math.h>
int main() {
  if (STACK_SIZE_64 < 512) fprintf(stderr, "Are you SURE a stack of %d is big enough for fray printfs?\n", STACK_SIZE_64);
  fray_block fb;

  //  fray(&fb, &poof, 0, MAX_FRAY_MEMBERS);
  // fray(&fb, &goof, 0, MAX_FRAY_MEMBERS);
  fray(&fb, &hoof, 0, MAX_FRAY_MEMBERS);

  double usec_tot = 0;
#define REPEAT 5
  int k;

  // create a random permutation in perm
  for(int i=0;i<TABLE_SIZE;i++) {
    perm[i] = i;
  }

  for(int i=0;i<TABLE_SIZE-1;i++) {
    uint64_t rnd = i+lrand48()%(TABLE_SIZE-i);
    uint64_t x = perm[i];
    perm[i] = perm[rnd];
    perm[rnd] = x;
  }

  // set data to a random cycle
  for(int i=0; i<TABLE_SIZE; i++) {
    a[perm[i]] = (uint64_t)(a + perm[(i+1)%TABLE_SIZE]);
  } 

   for (k = (int) MAX_FRAY_MEMBERS; k > 0; k--) {
    usec_tot = 0;
    int j;
    for (j = 0; j < REPEAT; j++) {
        struct timeval tv_start, tv_end;
        gettimeofday(&tv_start, 0);
        fray(&fb, &chase, 0, k);
        gettimeofday(&tv_end, 0);
        double usec = (tv_end.tv_sec - tv_start.tv_sec)*1000000 + (tv_end.tv_usec - tv_start.tv_usec);
        fprintf(stderr, "f&s(%d) ( %g ns/iter): %d iters in %g secs.\n", k, 1000*usec/NUM_SWAPS, NUM_SWAPS, usec/1000000);
        usec_tot += usec;
    }
    fprintf(stdout, "%d: %g ", k, 1000*usec_tot/NUM_SWAPS/REPEAT);
  }
  return 0;
}
