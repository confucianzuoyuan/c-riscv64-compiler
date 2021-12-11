#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>

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
    if (*p == '+' || *p == '-') {
      cur = cur->next = new_token(TK_PUNCT, p, p + 1);
      p++;
      continue;
    }

    error_at(p, "无效的标记");
  }

  cur = cur->next = new_token(TK_EOF, p, p);
  return head.next;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    error("%s: 参数数量不正确", argv[0]);
  }

  current_input = argv[1];
  Token *tok = tokenize();

  printf(".global main\n");
  printf("main:\n");

  // 第一个标记必须是数值
  printf("  li a0, %d\n", get_number(tok));
  tok = tok->next;

  // 后面跟着的是 `+ <number>` 或者 `- <number>` 。
  while (tok->kind != TK_EOF) {
    if (equal(tok, "+")) {
      printf("  addi a0, a0, %d\n", get_number(tok->next));
      tok = tok->next->next;
      continue;
    }

    tok = skip(tok, "-");
    printf("  addi a0, a0, -%d\n", get_number(tok));
    tok = tok->next;
  }
  
  return 0;
}
