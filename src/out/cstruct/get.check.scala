#include <stdbool.h>
#include <stdlib.h>

int snippet(struct Foo * x0);
int snippet(struct Foo * x0) {
  int x1 = x0->x;
  return x1;
}

