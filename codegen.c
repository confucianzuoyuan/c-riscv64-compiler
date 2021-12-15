#include "zhizhicc.h"

//
// 代码生成
//

static int depth;

static void push(void) {
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
  depth++;
}

static void pop(char *arg) {
  printf("  ld %s, 0(sp)\n", arg);
  printf("  addi sp, sp, 8\n");
  depth--;
}

// 计算给定节点在内存中的绝对地址
static void gen_addr(Node *node) {
  if (node->kind == ND_VAR) {
    // 计算变量的偏移量
    int offset = (node->name - 'a' + 1) * 8;
    printf("  addi a0, fp, %d\n", -offset);
    return;
  }

  error("不是一个左值");
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
  case ND_VAR:
    gen_addr(node);
    printf("  ld a0, 0(a0)\n");
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    pop("a1");
    printf("  sd a0, 0(a1)\n");
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

static void gen_stmt(Node *node) {
  if (node->kind == ND_EXPR_STMT) {
      gen_expr(node->lhs);
      return;
  }

  error("无效的语句");
}

void codegen(Node *node) {
  printf(".global main\n");
  printf("main:\n");

  // Prologue
  printf("  addi sp, sp, -8\n");
  printf("  sd fp, 0(sp)\n");
  printf("  mv fp, sp\n");
  printf("  addi sp, sp, -208\n");

  for (Node *n = node; n; n = n->next) {
      gen_stmt(n);
      assert(depth == 0);
  }

  // Epilogue
  printf("  mv sp, fp\n");
  printf("  ld fp, 0(sp)\n");
  printf("  addi sp, sp, 8\n");
  printf("  jr ra\n");
}