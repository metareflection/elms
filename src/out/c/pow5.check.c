#include <stdbool.h>
#include <stdlib.h>

int snippet(int x0);
int snippet(int x0) {
  int x1 = 1;
  int x2 = x0 * x1;
  int x3 = x0 * x2;
  int x4 = x0 * x3;
  int x5 = x0 * x4;
  int x6 = x0 * x5;
  return x6;
}

