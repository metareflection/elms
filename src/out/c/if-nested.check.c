#include <stdbool.h>
#include <stdlib.h>

int snippet(bool x0);
int snippet(bool x0) {
  int x5 = (x0 ? ({
    int x3 = (x0 ? ({
      int x1 = 1;
      x1;
    }) : ({
      int x2 = 2;
      x2;
    }));
    x3;
  }) : ({
    int x4 = 0;
    x4;
  }));
  return x5;
}

