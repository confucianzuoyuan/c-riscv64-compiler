#include "zhizhicc.h"

//
// 代码生成
//

static int depth;

static int count(void) {
  static int i = 1;
  return i++;
}

// 将a0寄存器中的内容压栈
static void push(void) {
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
  depth++;
}

// 将栈顶的内容弹栈到arg寄存器中
static void pop(char *arg) {
  printf("  ld %s, 0(sp)\n", arg);
  printf("  addi sp, sp, 8\n");
  depth--;
}

// 将 `n` 增加到最近的 `align` 的整数倍。
// 例如：align_to(5, 8)  => 8
//       align_to(11, 8) => 16
// 用来做内存对齐
static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

// 计算给定节点在内存中的绝对地址
static void gen_addr(Node *node) {
  if (node->kind == ND_VAR) {
    printf("  addi a0, fp, %d\n", node->var->offset);
    return;
  }

  error_tok(node->tok, "不是一个左值");
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

  error_tok(node->tok, "无效的表达式");
}

static void gen_stmt(Node *node) {
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    printf("  ble a0, zero, .L.else.%d\n", c);
    gen_stmt(node->then);
    printf("  j .L.end.%d\n", c);
    printf(".L.else.%d:\n", c);
    if (node->els)
      gen_stmt(node->els);
    printf(".L.end.%d:\n", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    printf(".L.begin.%d:\n", c);
    if (node->cond) {
      gen_expr(node->cond);
      printf("  ble a0, zero, .L.end.%d\n", c);
    }
    gen_stmt(node->then);
    if (node->inc) {
      gen_expr(node->inc);
    }
    printf("  j .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
    return;
  }
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_RETURN:
    gen_expr(node->lhs);
    printf("  j .L.return\n");
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  }

  error_tok(node->tok, "无效的语句");
}

// 为每个局部变量计算偏移量
static void assign_lvar_offsets(Function *prog) {
  int offset = 0;
  for (Obj *var = prog->locals; var; var = var->next) {
    offset += 8;
    var->offset = -offset;
  }
  prog->stack_size = align_to(offset, 16);
}

void codegen(Function *prog) {
  assign_lvar_offsets(prog);

  printf(".global main\n");
  printf("main:\n");

  // Prologue
  printf("  addi sp, sp, -8\n");
  printf("  sd fp, 0(sp)\n");
  printf("  mv fp, sp\n");
  printf("  addi sp, sp, -208\n");

  gen_stmt(prog->body);
  assert(depth == 0);

  // Epilogue
  printf(".L.return:\n");
  printf("  mv sp, fp\n");
  printf("  ld fp, 0(sp)\n");
  printf("  addi sp, sp, 8\n");
  printf("  jr ra\n");
}