#include "zhizhicc.h"

//
// 代码生成
//

static int depth;
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
static Obj *current_fn;

static void gen_expr(Node *node);

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
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      printf("  addi a0, fp, %d\n", node->var->offset);
    } else {
      // 将全局变量的地址加载到a0寄存器
      printf("  la a0, %s\n", node->var->name);
    }
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  }

  error_tok(node->tok, "不是一个左值");
}

static void load(Type *ty) {
  if (ty->kind == TY_ARRAY) {
    return;
  }
  if (ty->size == 1)
    printf("  lbu a0, 0(a0)\n");
  else
    printf("  ld a0, 0(a0)\n");
}

static void store(Type *ty) {
  pop("a1");

  if (ty->size == 1)
    printf("  sb a0, 0(a1)\n");
  else
    printf("  sd a0, 0(a1)\n");
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
    load(node->ty);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    store(node->ty);
    return;
  case ND_FUNCALL: {
    int nargs = 0;
    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_expr(arg);
      push();
      nargs++;
    }

    for (int i = nargs - 1; i >= 0; i--)
      pop(argreg[i]);

    printf("  call %s\n", node->funcname);
    return;
  }
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
    printf("  j .L.return.%s\n", current_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  }

  error_tok(node->tok, "无效的语句");
}

// 为每个局部变量计算偏移量
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    int offset = 0;
    for (Obj *var = fn->locals; var; var = var->next) {
      offset += var->ty->size;
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

// 产生数据段的内容，一般用来存放全局变量之类的数据。
static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function)
      continue;

    printf("  .data\n");
    printf("  .global %s\n", var->name);
    printf("%s:\n", var->name);
    
    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++)
        printf("  .byte %d\n", var->init_data[i]);
    } else {
      printf("  .zero %d\n", var->ty->size);
    }
  }
}

// 产生代码段的内容，存放程序
static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    printf(".global %s\n", fn->name);
    printf(".text\n");
    printf("%s:\n", fn->name);
    current_fn = fn;

    // Prologue
    printf("  addi sp, sp, -16\n");
    printf("  sd ra, 8(sp)\n"); // 保存返回值地址
    printf("  sd fp, 0(sp)\n"); // 保存帧指针
    printf("  mv fp, sp\n");
    printf("  addi sp, sp, %d\n", -fn->stack_size);

    // 将从寄存器传递过来的参数压栈
    int i = 0;
    for (Obj *var = fn->params; var; var = var->next)
      if (var->ty->size == 1)
        printf("  sb %s, %d(fp)\n", argreg[i++], var->offset);
      else
        printf("  sd %s, %d(fp)\n", argreg[i++], var->offset);

    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue
    printf(".L.return.%s:\n", fn->name);
    printf("  mv sp, fp\n");
    printf("  ld ra, 8(sp)\n"); // 恢复返回值地址
    printf("  ld fp, 0(sp)\n"); // 恢复帧指针
    printf("  addi sp, sp, 16\n");
    printf("  ret\n");
  }
}

void codegen(Obj *prog) {
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}