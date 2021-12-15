#include "zhizhicc.h"

int main(int argc, char **argv) {
  if (argc != 2) {
    error("%s: 参数数量不正确", argv[0]);
  }
  
  Token *tok = tokenize(argv[1]);
  Function *prog = parse(tok);

  // 后序遍历AST，产生汇编代码
  codegen(prog);

  return 0;
}
