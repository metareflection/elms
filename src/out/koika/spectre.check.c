
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
struct StateT * x20(struct StateT * x21);
struct StateT * x7(struct StateT * x8);
struct StateT * x32(struct StateT * x33);
struct StateT * x13(struct StateT * x14);
struct StateT * snippet(struct StateT * x0) {
  struct StateT * x92 = x1(x0);
  return x92;
}

struct StateT * x1(struct StateT * x2) {
  int x3 = x2->timer;
  int x86 = x3 + 1;
  x2->timer = x86;int * x5 = x2->regs;
  x5[3] = 0;
  struct StateT * x84 = x7(x2);
  return x84;
}

struct StateT * x20(struct StateT * x21) {
  int x22 = x21->timer;
  int x53 = x22 + 1;
  x21->timer = x53;int * x24 = x21->regs;
  int x25 = x24[3];
  int * x26 = x21->regs;
  int x27 = x26[0];
  int * x28 = x21->mem;
  int x59 = x25 + x27;
  int x29 = x28[x59];
  int * x30 = x21->regs;
  x30[1] = x29;
  struct StateT * x51 = x32(x21);
  return x51;
}

struct StateT * x7(struct StateT * x8) {
  int x9 = x8->timer;
  int x78 = x9 + 1;
  x8->timer = x78;int * x11 = x8->regs;
  x11[0] = 20;
  struct StateT * x76 = x13(x8);
  return x76;
}

struct StateT * x32(struct StateT * x33) {
  int x34 = x33->timer;
  int x43 = x34 + 1;
  x33->timer = x43;int * x36 = x33->regs;
  int x37 = x36[1];
  int * x38 = x33->mem;
  int x39 = x38[x37];
  int * x40 = x33->regs;
  x40[2] = x39;
  return x33;
}

struct StateT * x13(struct StateT * x14) {
  int x15 = x14->timer;
  int x69 = x15 + 1;
  x14->timer = x69;int * x17 = x14->regs;
  int x18 = x17[0];
  bool x72 = x18 >= 20;
  struct StateT * x67 = (x72 ? x14 : ({
    struct StateT * x65 = x20(x14);
    x65;
  }));
  return x67;
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