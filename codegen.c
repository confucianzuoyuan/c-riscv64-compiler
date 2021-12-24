#include "zhizhicc.h"

//
// 代码生成
//

static int depth;
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
static Obj *current_fn;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
  printf("\n");
}

static int count(void) {
  static int i = 1;
  return i++;
}

// 将a0寄存器中的内容压栈
static void push(void) {
  println("  addi sp, sp, -8");
  println("  sd a0, 0(sp)");
  depth++;
}

// 将栈顶的内容弹栈到arg寄存器中
static void pop(char *arg) {
  println("  ld %s, 0(sp)", arg);
  println("  addi sp, sp, 8");
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
      println("  addi a0, fp, %d", node->var->offset);
    } else {
      // 将全局变量的地址加载到a0寄存器
      println("  la a0, %s", node->var->name);
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
    println("  lbu a0, 0(a0)");
  else
    println("  ld a0, 0(a0)");
}

static void store(Type *ty) {
  pop("a1");

  if (ty->size == 1)
    println("  sb a0, 0(a1)");
  else
    println("  sd a0, 0(a1)");
}

static void gen_expr(Node *node) {
  switch (node->kind) {
  case ND_NUM:
    println("  li a0, %d", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    println("  sub a0, zero, a0");
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
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
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

    println("  call %s", node->funcname);
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
    println("  add a0, a0, a1");
    return;
  case ND_SUB:
    println("  sub a0, a1, a0");
    return;
  case ND_MUL:
    println("  mul a0, a0, a1");
    return;
  case ND_DIV:
    println("  div a0, a1, a0");
    return;
  case ND_EQ:
    println("  sub a0, a1, a0");
    println("  seqz a0, a0");
    return;
  case ND_NE:
    println("  sub a0, a1, a0");
    println("  snez a0, a0");
    return;
  case ND_LT:
    println("  slt a0, a1, a0");
    return;
  case ND_LE:
    println("  sgt a0, a1, a0");
    println("  xori a0, a0, 1");
    return;
  }

  error_tok(node->tok, "无效的表达式");
}

static void gen_stmt(Node *node) {
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    println("  ble a0, zero, .L.else.%d", c);
    gen_stmt(node->then);
    println("  j .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els)
      gen_stmt(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      println("  ble a0, zero, .L.end.%d", c);
    }
    gen_stmt(node->then);
    if (node->inc) {
      gen_expr(node->inc);
    }
    println("  j .L.begin.%d", c);
    println(".L.end.%d:", c);
    return;
  }
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_RETURN:
    gen_expr(node->lhs);
    println("  j .L.return.%s", current_fn->name);
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

    println("  .data");
    println("  .global %s", var->name);
    println("%s:", var->name);
    
    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++)
        println("  .byte %d", var->init_data[i]);
    } else {
      println("  .zero %d", var->ty->size);
    }
  }
}

// 产生代码段的内容，存放程序
static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    println(".global %s", fn->name);
    println(".text");
    println("%s:", fn->name);
    current_fn = fn;

    // Prologue
    println("  addi sp, sp, -16");
    println("  sd ra, 8(sp)"); // 保存返回值地址
    println("  sd fp, 0(sp)"); // 保存帧指针
    println("  mv fp, sp");
    println("  addi sp, sp, %d", -fn->stack_size);

    // 将从寄存器传递过来的参数压栈
    int i = 0;
    for (Obj *var = fn->params; var; var = var->next)
      if (var->ty->size == 1)
        println("  sb %s, %d(fp)", argreg[i++], var->offset);
      else
        println("  sd %s, %d(fp)", argreg[i++], var->offset);

    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue
    println(".L.return.%s:", fn->name);
    println("  mv sp, fp");
    println("  ld ra, 8(sp)"); // 恢复返回值地址
    println("  ld fp, 0(sp)"); // 恢复帧指针
    println("  addi sp, sp, 16");
    println("  ret");
  }
}

void codegen(Obj *prog) {
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}