#include "zhizhicc.h"

//
// 代码生成
//

static int depth;

static void push(void) {
  printf("  addi sp, sp, -8\n");
  printf("  sw a0, 0(sp)\n");
  depth++;
}

static void pop(char *arg) {
  printf("  lw %s, 0(sp)\n", arg);
  printf("  addi sp, sp, 8\n");
  depth--;
}

static void gen_expr(Node *node) {
  switch (node->kind) {
  case ND_NUM:
    printf("  li a0, %d\n", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    printf("  sub a0, zero, a0\n");
    return;
  }

  gen_expr(node->lhs);
  // lhs的计算结果push到栈上。
  push();
  gen_expr(node->rhs);
  // lhs的计算结果pop到a1寄存器中。
  // rhs的计算结果在a0寄存器中。
  pop("a1");

  switch (node->kind) {
  case ND_ADD:
    printf("  add a0, a0, a1\n");
    return;
  case ND_SUB:
    printf("  sub a0, a1, a0\n");
    return;
  case ND_MUL:
    printf("  mul a0, a0, a1\n");
    return;
  case ND_DIV:
    printf("  div a0, a1, a0\n");
    return;
  case ND_EQ:
    printf("  sub a0, a1, a0\n");
    printf("  seqz a0, a0\n");
    return;
  case ND_NE:
    printf("  sub a0, a1, a0\n");
    printf("  snez a0, a0\n");
    return;
  case ND_LT:
    printf("  slt a0, a1, a0\n");
    return;
  case ND_LE:
    printf("  sgt a0, a1, a0\n");
    printf("  xori a0, a0, 1\n");
    return;
  }

  error("无效的表达式");
}

void codegen(Node *node) {
  printf(".global main\n");
  printf("main:\n");

  gen_expr(node);
  
  assert(depth == 0);
}