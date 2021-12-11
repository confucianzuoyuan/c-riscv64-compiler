#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "%s: 参数数量无效\n", argv[0]);
    return 1;
  }

  char *p = argv[1];

  printf(".global main\n");
  printf("main:\n");
  printf("  li a0, %ld\n", strtol(p, &p, 10));

  while (*p) {
    if (*p == '+') {
      p++;
      printf("  addi a0, a0, %ld\n", strtol(p, &p, 10));
      continue;
    }

    if (*p == '-') {
      p++;
      printf("  addi a0, a0, -%ld\n", strtol(p, &p, 10));
      continue;
    }

    fprintf(stderr, "未识别字符：'%c'\n", *p);
    return 1;
  }
  return 0;
}
