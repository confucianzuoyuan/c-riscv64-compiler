#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "%s: 参数数量无效\n", argv[0]);
    return 1;
  }

  printf(".global main\n");
  printf("main:\n");
  printf("  li a0, %d\n", atoi(argv[1]));
  return 0;
}
