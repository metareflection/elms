
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

struct StateT * x19(struct StateT * x20);
struct StateT * x25(struct StateT * x26);
struct StateT * x70(struct StateT * x71);
struct StateT * x38(struct StateT * x39);
struct StateT * x91(struct StateT * x92);
struct StateT * snippet(struct StateT * x0);
struct StateT * x50(struct StateT * x51);
struct StateT * x62(struct StateT * x63);
struct StateT * x1(struct StateT * x2);
struct StateT * x76(struct StateT * x77);
struct StateT * x7(struct StateT * x8);
struct StateT * x13(struct StateT * x14);
struct StateT * x99(struct StateT * x100);
struct StateT * x19(struct StateT * x20) {
  int x21 = x20->timer;
  int x165 = x21 + 1;
  x20->timer = x165;int * x23 = x20->regs;
  int x24 = x23[4];
  bool x168 = x24 >= 4;
  struct StateT * x163 = (x168 ? ({
    struct StateT * x36 = x25(x20);
    x36;
  }) : ({
    struct StateT * x161 = x38(x20);
    x161;
  }));
  return x163;
}

struct StateT * x25(struct StateT * x26) {
  int x27 = x26->timer;
  int x32 = x27 + 1;
  x26->timer = x32;int * x29 = x26->regs;
  x29[0] = 1;
  return x26;
}

struct StateT * x70(struct StateT * x71) {
  int x72 = x71->timer;
  int x84 = x72 + 1;
  x71->timer = x84;int * x74 = x71->regs;
  x74[0] = 0;
  struct StateT * x82 = x76(x71);
  return x82;
}

struct StateT * x38(struct StateT * x39) {
  int x40 = x39->timer;
  int x149 = x40 + 1;
  x39->timer = x149;int * x42 = x39->regs;
  int x43 = x42[2];
  int * x44 = x39->regs;
  int x45 = x44[4];
  int * x46 = x39->mem;
  int x155 = x43 + x45;
  int x47 = x46[x155];
  int * x48 = x39->regs;
  x48[0] = x47;
  struct StateT * x147 = x50(x39);
  return x147;
}

struct StateT * x91(struct StateT * x92) {
  int x93 = x92->timer;
  int x111 = x93 + 1;
  x92->timer = x111;int * x95 = x92->regs;
  int x96 = x95[4];
  int * x97 = x92->regs;
  int x115 = x96 + 1;
  x97[4] = x115;
  struct StateT * x109 = x99(x92);
  return x109;
}

struct StateT * snippet(struct StateT * x0) {
  struct StateT * x198 = x1(x0);
  return x198;
}

struct StateT * x50(struct StateT * x51) {
  int x52 = x51->timer;
  int x135 = x52 + 1;
  x51->timer = x135;int * x54 = x51->regs;
  int x55 = x54[3];
  int * x56 = x51->regs;
  int x57 = x56[4];
  int * x58 = x51->mem;
  int x141 = x55 + x57;
  int x59 = x58[x141];
  int * x60 = x51->regs;
  x60[1] = x59;
  struct StateT * x133 = x62(x51);
  return x133;
}

struct StateT * x62(struct StateT * x63) {
  int x64 = x63->timer;
  int x122 = x64 + 1;
  x63->timer = x122;int * x66 = x63->regs;
  int x67 = x66[0];
  int * x68 = x63->regs;
  int x69 = x68[1];
  bool x127 = !(x67 == x69);
  struct StateT * x120 = (x127 ? ({
    struct StateT * x89 = x70(x63);
    x89;
  }) : ({
    struct StateT * x118 = x91(x63);
    x118;
  }));
  return x120;
}

struct StateT * x1(struct StateT * x2) {
  int x3 = x2->timer;
  int x192 = x3 + 1;
  x2->timer = x192;int * x5 = x2->regs;
  x5[2] = 0;
  struct StateT * x190 = x7(x2);
  return x190;
}

struct StateT * x76(struct StateT * x77) {
  int x78 = x77->timer;
  int x81 = x78 + 1;
  x77->timer = x81;return x77;
}

struct StateT * x7(struct StateT * x8) {
  int x9 = x8->timer;
  int x184 = x9 + 1;
  x8->timer = x184;int * x11 = x8->regs;
  x11[3] = 20;
  struct StateT * x182 = x13(x8);
  return x182;
}

struct StateT * x13(struct StateT * x14) {
  int x15 = x14->timer;
  int x176 = x15 + 1;
  x14->timer = x176;int * x17 = x14->regs;
  x17[4] = 0;
  struct StateT * x174 = x19(x14);
  return x174;
}

struct StateT * x99(struct StateT * x100) {
  int x101 = x100->timer;
  int x106 = x101 + 1;
  x100->timer = x106;struct StateT * x104 = x19(x100);
  return x104;
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