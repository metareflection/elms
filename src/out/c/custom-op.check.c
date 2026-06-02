#include <stdbool.h>
#include <stdlib.h>

const char * snippet(int x0);
const char * snippet(int x0) {
  const char * x1 = "world";
  const char * x2 = hello(x0, x1);
  return x2;
}

