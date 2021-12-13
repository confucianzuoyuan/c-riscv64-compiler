#include "zhizhicc.h"

int main(int argc, char **argv) {
  if (argc != 2) {
    error("%s: 参数数量不正确", argv[0]);
  }
  
  Token *tok = tokenize(argv[1]);
  Node *node = parse(tok);
  codegen(node);

  return 0;
}
