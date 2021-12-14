#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
Token *tokenize(char *input);

//
// 语法分析器
//

typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NEG, // unary -
  ND_EQ,  // ==
  ND_NE,  // !=
  ND_LT,  // <
  ND_LE,  // <=
  ND_EXPR_STMT, // 表达式语句
  ND_NUM, // 整数
} NodeKind;

// 抽象语法树节点类型
typedef struct Node Node;
struct Node {
  NodeKind kind; // 节点类型
  Node *next;    // 下一个节点的指针
  Node *lhs;     // 运算符左边的节点
  Node *rhs;     // 运算符右边的节点
  int val;       // 如果kind == ND_NUM，则使用这个字段
};

Node *parse(Token *tok);

//
// 代码生成
//

void codegen(Node *node);