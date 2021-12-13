#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

//
// 词法分析器
//

typedef enum {
  TK_PUNCT, // 分隔符
  TK_NUM,   // 数值字面量
  TK_EOF,   // 文件结束符标记
} TokenKind;

// 标记类型
typedef struct Token Token;
struct Token {
  TokenKind kind; // 标记类型
  Token *next;    // 下一个标记的指针
  int val;        // 如果标记是TK_NUM，它的值
  char *loc;      // 标记的位置
  int len;        // 标记的长度
};

// 输入的字符串
static char *current_input;

// 报错然后退出
static void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

// 报错并指出错误出现的位置，然后退出
static void verror_at(char *loc, char *fmt, va_list ap) {
  int pos = loc - current_input;
  fprintf(stderr, "%s\n", current_input);
  // 打印出错位置前面的空格
  fprintf(stderr, "%*s", pos, "");
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

static void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(loc, fmt, ap);
}

static void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->loc, fmt, ap);
}

// 如果当前标记可以匹配`s`，那么消费掉
static bool equal(Token *tok, char *op) {
  return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

// 确保当前标记是`s`，然后跳过
static Token *skip(Token *tok, char *s) {
  if (!equal(tok, s)) error_tok(tok, "预期的字符串是：'%s'", s);
  return tok->next;
}

// 确保当前标记是TK_NUM，然后获取数值
static int get_number(Token *tok) {
  if (tok->kind != TK_NUM) error_tok(tok, "预期是一个数值");
  return tok->val;
}

// 创建一个新的标记
static Token *new_token(TokenKind kind, char *start, char *end) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->loc = start;
  tok->len = end - start;
  return tok;
}

// 对`p`指针指向的字符串进行词法分析，然后返回新的标记序列。
static Token *tokenize(void) {
  char *p = current_input;
  Token head = {};
  Token *cur = &head;

  while (*p) {
    // 跳过空白字符
    if (isspace(*p)) {
      p++;
      continue;
    }

    // 数值字面量
    if (isdigit(*p)) {
      cur = cur->next = new_token(TK_NUM, p, p);
      char *q = p;
      // strtoul: 将字符串转换成无符号长整型；string to unsigned long
      cur->val = strtoul(p, &p, 10);
      cur->len = p - q;
      continue;
    }

    // 分隔符
    if (ispunct(*p)) {
      cur = cur->next = new_token(TK_PUNCT, p, p + 1);
      p++;
      continue;
    }

    error_at(p, "无效的标记");
  }

  cur = cur->next = new_token(TK_EOF, p, p);
  return head.next;
}

//
// 语法分析器
//

typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NEG, // unary -
  ND_NUM, // 整数
} NodeKind;

// 抽象语法树节点类型
typedef struct Node Node;
struct Node {
  NodeKind kind; // 节点类型
  Node *lhs;     // 运算符左边的节点
  Node *rhs;     // 运算符右边的节点
  int val;       // 如果kind == ND_NUM，则使用这个字段
};

// 创建新的节点
static Node *new_node(NodeKind kind) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  return node;
}

// 创建新的二元表达式节点
static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs) {
  Node *node = new_node(kind);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

// 创建新的一元表达式节点
static Node *new_unary(NodeKind kind, Node *expr) {
  Node *node = new_node(kind);
  node->lhs = expr;
  return node;
}

// 创建数值字面量节点
static Node *new_num(int val) {
  Node *node = new_node(ND_NUM);
  node->val = val;
  return node;
}

static Node *expr(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// expr = mul ("+" mul | "-" mul)*
static Node *expr(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    if (equal(tok, "+")) {
      node = new_binary(ND_ADD, node, mul(&tok, tok->next));
      continue;
    }

    if (equal(tok, "-")) {
      node = new_binary(ND_SUB, node, mul(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = unary ("*" unary | "/" unary)*
static Node *mul(Token **rest, Token *tok) {
  Node *node = unary(&tok, tok);

  for (;;) {
    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, unary(&tok, tok->next));
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, unary(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

// unary = ("+" | "-") unary
//       | primary
static Node *unary(Token **rest, Token *tok) {
  if (equal(tok, "+"))
    return unary(rest, tok->next);

  if (equal(tok, "-"))
    return new_unary(ND_NEG, unary(rest, tok->next));

  return primary(rest, tok);
}

// primary = "(" expr ")" | num
static Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(")) {
    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (tok->kind == TK_NUM) {
    Node *node = new_num(tok->val);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "预期是一个表达式");
}

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
  // 栈顶的计算结果pop到a1寄存器中。
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
  }

  error("无效的表达式");
}

int main(int argc, char **argv) {
  if (argc != 2) {
    error("%s: 参数数量不正确", argv[0]);
  }

  current_input = argv[1];
  Token *tok = tokenize();
  Node *node = expr(&tok, tok);

  if (tok->kind != TK_EOF) {
    error_tok(tok, "多余的标记");
  }

  printf(".global main\n");
  printf("main:\n");

  gen_expr(node);
  
  assert(depth == 0);

  return 0;
}
