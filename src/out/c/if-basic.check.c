#include <stdbool.h>
#include <stdlib.h>

int snippet(bool x0);
int snippet(bool x0) {
  int x3 = (x0 ? ({
    int x1 = 1;
    x1;
  }) : ({
    int x2 = 0;
    x2;
  }));
  return x3;
}

