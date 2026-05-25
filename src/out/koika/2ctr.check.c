
#define NUM_REGS 8
#define MEM_SIZE 30
#define SECRET_SIZE 10
#define SECRET_OFFSET 20
#define CACHE_LRU_SIZE 0
#ifndef CBMC
#define __CPROVER_assert(b,s) 0
#define nondet_uint() 0
#else
int nondet_uint();
#endif
int bounded(int low, int high) {
  int x = nondet_uint();
  __CPROVER_assume(low <= x && x <= high);
  return x;
}
struct StateT {
  int regs[NUM_REGS];
  int mem[MEM_SIZE];
  int saved_regs[NUM_REGS];
  int cache_keys[CACHE_LRU_SIZE];
  int cache_vals[CACHE_LRU_SIZE];
  int timer;
};

void init(struct StateT *s) {
  for (int i=0; i<NUM_REGS; i++) {
    s->regs[i] = 0;
  }
  s->timer = 0;
  for (int i=0; i<MEM_SIZE; i++) {
    s->mem[i] = 0;
  }
}

/*****************************************
Emitting C Generated Code
*******************************************/

#include <stdbool.h>
#include <stdlib.h>

struct StateT * snippet(struct StateT * x0);
struct StateT * x1(struct StateT * x2);
struct StateT * x8(struct StateT * x9);
struct StateT * x18(struct StateT * x19);
struct StateT * snippet(struct StateT * x0) {
  struct StateT * x60 = x1(x0);
  return x60;
}

struct StateT * x1(struct StateT * x2) {
  int x3 = x2->timer;
  int x53 = x3 + 1;
  x2->timer = x53;int * x5 = x2->regs;
  int x6 = x5[0];
  bool x56 = x6 == 0;
  struct StateT * x51 = (x56 ? x2 : ({
    struct StateT * x49 = x8(x2);
    x49;
  }));
  return x51;
}

struct StateT * x8(struct StateT * x9) {
  int x10 = x9->timer;
  int x39 = x10 + 1;
  x9->timer = x39;int * x12 = x9->regs;
  int x13 = x12[0];
  int * x14 = x9->mem;
  int x15 = x14[x13];
  int * x16 = x9->regs;
  x16[1] = x15;
  struct StateT * x37 = x18(x9);
  return x37;
}

struct StateT * x18(struct StateT * x19) {
  int x20 = x19->timer;
  int x29 = x20 + 1;
  x19->timer = x29;int * x22 = x19->regs;
  int x23 = x22[1];
  int * x24 = x19->mem;
  int x33 = x23 + 4;
  int x25 = x24[x33];
  int * x26 = x19->regs;
  x26[2] = x25;
  return x19;
}



/*****************************************
End of C Generated Code
*******************************************/

int main(int argc, char* argv[]) {
  struct StateT s1, s2;
  init(&s1);
  init(&s2);
  
  int x = bounded(0, 20);
  s1.regs[0] = x;
  s2.regs[0] = x;
  
  // initialize secret
  for (int i=0; i<SECRET_SIZE; i++) {
    s1.mem[SECRET_OFFSET+i] = bounded(0, 20);
    s2.mem[SECRET_OFFSET+i] = bounded(0, 20);
  }
  struct StateT *s1_ = snippet(&s1);
  struct StateT *s2_ = snippet(&s2);
  __CPROVER_assert(s1_->timer==s2_->timer, "timing leak");
  return 0;
}