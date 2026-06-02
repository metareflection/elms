#include <stdbool.h>
#include <stdlib.h>

int snippet(int x0);
int snippet(int x0) {
  int x1 = 1;
  bool x2 = x0 == x1;
  int x4 = (x2 ? ({
    int x3 = 1;
    x3;
  }) : x0);
  return x4;
}

