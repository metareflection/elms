#include <stdbool.h>
#include <stdlib.h>

bool snippet(void x0);
bool snippet(void x0) {
  const char * x1 = "a";
  printf("%s\n", x1);
  bool x3 = false;
  bool x7 = x3 && (({
    const char * x4 = "b";
    printf("%s\n", x4);
    bool x6 = true;
    x6;
  }));
  const char * x8 = "a";
  printf("%s\n", x8);
  bool x10 = true;
  bool x14 = x10 || (({
    const char * x11 = "b";
    printf("%s\n", x11);
    bool x13 = false;
    x13;
  }));
  bool x15 = x7 && x14;
  return x15;
}

