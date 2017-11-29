#ifndef FRAY_H
#define FRAY_H
#define COMPILED_WITH_FRAME_POINTERS
#define MAX_FRAY_MEMBERS 8
//Beware making stack too small yet expecting frays to execute printf!
#define STACK_SIZE_64 4096
#include <stdint.h>

//#define DEBUG
#if defined(DEBUG) || defined(FRAY_STATS)
#include <stdio.h>
#endif
#define blue_doo(x) #x ": "
#define voo_doo(x) blue_doo(x)
#define at_line() voo_doo(__LINE__)

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

// Main concept is that state is represented by array sptrs of stack pointers, one for
// each instantiated fray member.
// The array is partitioned into three regions whose extents are indexed by
//   0 <=sched <= alive < MAX_FRAY_MEMBERS
// Stack pointers to the left of sched are "scheduled"; in particular, that pointed to by
// s_i is the only member "running".
// Stack pointers to the left of alive belong to frays that have not executed fray_return.
// Stack pointers to right of alive-1 are unused.
//
// Scheduling is accomplished by iterating s_i cyclically through the range 0 <= sched.
// Context switch from one stack pointer to another is accomplished via swap_frays(from, to)
// To yield, exchange sptrs[s_i] with sptrs[s_i = (s_i+1)mod sched]
// To block, exchange sptrs[s_i] with sptrs[--sched] and swap_frays from sched to s_i.
// Etc...  simple as snow, eh? 

#ifdef __cplusplus
class fray_block {
 public:
  volatile int sched; //number of fray members scheduled for execution; sp's in sptrs[0.. sched-1]
  volatile int alive; //number of fray members that have not yet executed fray_return; >= sched; sp's in sptrs[0.. alive-1]
  uint64_t stacks[STACK_SIZE_64*MAX_FRAY_MEMBERS];//1024*8];
  uint64_t * sptrs[MAX_FRAY_MEMBERS+1]; //[MAX_FRAY_MEMBERS] is main thread's sp
  volatile int s_i; //index into sptrs where current stack pointer lives
  int int64_per_stack;
  int err; //0 initialized to zero; set by user in calls to fray_exit
  void * data;
#ifdef FRAY_STATS
  volatile int64_t num_yields;
  volatile int64_t hist[MAX_FRAY_MEMBERS];
#endif
  fray_block() {
    //a yield by a not yet frayed thread should lead right back to itself
    sptrs[0] = &(stacks[STACK_SIZE_64*MAX_FRAY_MEMBERS - 1]);
    sched = alive = 0;
    s_i = 0;
    err = 0;
#ifdef FRAY_STATS
    num_yields = 0;
    for (int i = 0; i < MAX_FRAY_MEMBERS; i++) hist[i] = 0;
#endif
  }
};
#else
typedef struct {
  uint64_t stacks[STACK_SIZE_64*MAX_FRAY_MEMBERS];//1024*8];
  uint64_t * sptrs[MAX_FRAY_MEMBERS+1]; //[MAX_FRAY_MEMBERS] is main thread's sp
  volatile int sched; //number of fray members that haven't executed fray_return
  volatile int alive;
  volatile int s_i; //index into sptrs where current stack pointer lives
  int int64_per_stack;
  int err; //0 initialized to zero; set by user in calls to fray_exit
  void * data;
} fray_block;
#endif

extern fray_block * global_fb; //an ugly hack: one global fray to save painful call-tree mods during experimentation

EXTERN_C void swap_frays(uint64_t **from, uint64_t **to);

static void fray_yield(fray_block *fb) {
#ifdef DEBUG
  if (STACK_SIZE_64 >= 1024) {
    int i; for(i = 0; i < MAX_FRAY_MEMBERS; i++) fprintf(stderr, "%p ", fb->sptrs[i]); fprintf(stderr, "\n");
    fprintf(stderr, "yielding from %d... %p to %p\n", fb->s_i, fb->sptrs[fb->s_i], fb->sptrs[(fb->s_i+1 < fb->sched) ? (fb->s_i+1) : 0]);
  } else {
    fprintf(stderr, "fray.h fray_yield: fray stack too small to report full diagnostics\n");
  }
#endif
  int now = fb->s_i;
  int next = (now + 1 < fb->sched) ? now + 1 : 0;
  fb->s_i = next;
#ifdef FRAY_STATS
  fb->num_yields++;
  fb->hist[fb->sched-1]++;
#endif
  swap_frays(&fb->sptrs[now], &fb->sptrs[next]);
}
static void fray_yield(fray_block *fb)  __attribute__((always_inline));

typedef void (*voidfn)(fray_block *, int64_t);

static uint64_t fray_fetch_and_yield(fray_block *fb, const void *p)  __attribute__((unused, always_inline));
static uint64_t fray_fetch_and_yield(fray_block *fb, const void *p) {
  __builtin_prefetch(p,0,0);
  fray_yield(fb);
  return *((uint64_t *)p);
}
static void fray_prefetch_and_yield(fray_block *fb, const void *p)  __attribute__((unused, always_inline));
static void fray_prefetch_and_yield(fray_block *fb, const void *p) {
  __builtin_prefetch(p,0,0);
  fray_yield(fb);
}
static void fray_prefetch_L2_and_yield(fray_block *fb, const void *p)  __attribute__((unused, always_inline));
static void fray_prefetch_L2_and_yield(fray_block *fb, const void *p) {
  __builtin_prefetch(p,0,3);
  fray_yield(fb);
}
static void fray_prefetch_rw_and_yield(fray_block *fb, const void *p)  __attribute__((unused, always_inline));
static void fray_prefetch_rw_and_yield(fray_block *fb, const void *p) {
  __builtin_prefetch(p,1,1);
  fray_yield(fb);
}
static void stack_init(uint64_t *sp, voidfn f, uint64_t *arg0, uint64_t *arg1);
static void start_fray(void *rdi, void *rsi, void *rdx, void *rcx, void *r8, void *r9, //args in regs
                voidfn, void *arg0, void *arg1); //args on stack
// Entries on initial stack passed in to start_fray...
typedef enum regoff_t {
  _R15 = 0,
  _R14,
  _R13,
  _R12,
  _RBX,
  _RBP,
  _RPC,
  _OLD_BP,
  _ARG7,
  _ARG8,
  _ARG9,
  _REGOFF_N
} regoff_t;

static void start_fray(void *rdi, void *rsi, void *rdx, void *rcx, void *r8, void *r9, //args in regs
                voidfn f, void *arg0, void *arg1) //args on stack
{
  (*f)((fray_block *) arg0, (int64_t) arg1);
}

static void stack_init(uint64_t *sp, voidfn f,
                         //  void (*f)(void *varg0, void* varg1),
                uint64_t *arg0,
                uint64_t *arg1) {
  //# define CAST(x)  (int)x
# define CAST(x)  (uint64_t) x
  sp[_R15] = CAST(1);
  sp[_R14] = CAST(2);
  sp[_R13] = CAST(3);
  sp[_R12] = CAST(4);
  sp[_RBX] = CAST(5);
  sp[_RBP] = CAST(6);
  sp[_RPC] = (uint64_t) start_fray;
  uint64_t old_bp;
  asm("mov %%rbp, %0" : "=r" (old_bp));
  sp[_OLD_BP] = old_bp;
  //sp[_OLD_BP] = CAST(0); //thinking this would stop an unwind...  but it didn't
  sp[_ARG7] = (uint64_t) f;
  sp[_ARG8] = (int64_t) arg0;
  sp[_ARG9] = (uint64_t) arg1;
}

static int fray(fray_block *fb, voidfn fn, void * data, int sched)  __attribute__((unused));
static int fray(fray_block *fb, voidfn fn, void * data, int sched) {
  #ifdef DEBUG
  fprintf(stderr, "fray: %p %p %p %d \n", fb, fn, data, sched);
  #endif
  fb->int64_per_stack = STACK_SIZE_64;
  fb-> alive = fb->sched = (sched < MAX_FRAY_MEMBERS) ? sched : MAX_FRAY_MEMBERS;
  fb->data = data;
  fb->s_i = 0;
  fb->err = 0;
  int64_t i;
  for (i = 0; i < fb->alive; i++) {
    fb->sptrs[i] = &(fb->stacks[fb->int64_per_stack*(i+1) - _REGOFF_N]);
    stack_init(fb->sptrs[i], (voidfn)(fn), (uint64_t*)(fb),(uint64_t*)(i));
  }
  swap_frays(&fb->sptrs[MAX_FRAY_MEMBERS], &fb->sptrs[0]);
  return fb->err;
}

static void fray_exit(fray_block *fb, int err) __attribute__((unused));
static void fray_exit(fray_block *fb, int err) {
  fb->err = err;
  swap_frays(&fb->sptrs[fb->s_i], &fb->sptrs[MAX_FRAY_MEMBERS]);
}

static void fray_return(fray_block *fb) __attribute__((unused));
static void fray_return(fray_block *fb) {
#ifdef DEBUG
  fprintf(stderr, "fray_return: fb->sptrs[MAX_FRAY_MEMBERS] = %p\n", fb->sptrs[MAX_FRAY_MEMBERS]);
  fprintf(stderr, "fray_return: %p %d, %d remain\n", fb, fb->s_i, fb->sched);
#endif
  int n = --fb->alive;
  if (n) {
    int sched_last = --fb->sched;
    if (sched_last) { //replace sp with sp of scheduled last, scheduled last with last alive, & yield
      int s = fb->s_i;
      fb->sptrs[s] = fb->sptrs[sched_last];
      fb->sptrs[sched_last] = fb->sptrs[n];
      s = (s + 1 < sched_last) ? s + 1 : 0;
      fb->s_i = s;
      swap_frays(&fb->sptrs[fb->alive], &fb->sptrs[s]); //store state (needlessly) and switch to next
    }
    else { //someone alive, but nobody scheduled; sched<-alive; yield
      fb->sched = n;
      fb->sptrs[0] = fb->sptrs[n]; //stick last alive in my spot (I be done)
      swap_frays(&fb->sptrs[n], &fb->sptrs[0]); //store state (needlessly) and switch to 0th
    }
  }
  else fray_exit(fb, fb->err);
}

static int fray_partial(fray_block *fb) __attribute__((unused));
static int fray_partial(fray_block *fb) {
  return (fb->sched < MAX_FRAY_MEMBERS ? 1 : 0);
}

// Note: fray_barrier waits until all have either arrived at barrier or returned.
// If some members are in barrier and all others have returned, fray exits anyway.
static void fray_barrier(fray_block *fb) __attribute__((unused));
#include <stdio.h>
static void fray_barrier(fray_block *fb) {
#ifdef DEBUG
  fprintf(stderr, "fray_barrier: barrier %d s_i %d sched %d \n", fb->barrier, fb->s_i, fb->sched);
#endif
  int n = --fb->sched;
  if (n) {
    int s = fb->s_i;
    uint64_t * tmp = fb->sptrs[s];  // swap this fray's sp with n'th
    fb->sptrs[s] = fb->sptrs[n];
    fb->sptrs[n] = tmp;
    s = (s + 1 < fb->sched) ? s + 1 : 0;
    fb->s_i = s;
    swap_frays(&fb->sptrs[n], &fb->sptrs[s]);
  }
  else { //all have arrived at barrier (or returned)
    fb->sched = fb->alive;
    //continue on!
  }
}
#ifdef FRAY_STATS
static void fray_stats(fray_block *fb) __attribute__((unused));
static void fray_stats(fray_block *fb) {
  fprintf(stderr, "fray stats: %ld yields; sched ", fb->num_yields);
  for (int i = 0; i < MAX_FRAY_MEMBERS; i++) {
    fprintf(stderr, "%ld ", fb->hist[i]);
  }
  fprintf(stderr, "\n");
}
#endif
#endif // FRAY_H
