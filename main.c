#include "zhizhicc.h"

int main(int argc, char **argv) {
  if (argc != 2) {
    error("%s: 参数数量不正确", argv[0]);
  }
  
  Token *tok = tokenize_file(argv[1]);
  Obj *prog = parse(tok);

  // 后序遍历AST，产生汇编代码
  codegen(prog);

  return 0;
}
