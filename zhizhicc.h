#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node Node;

//
// 词法分析器
//

typedef enum {
  TK_IDENT,   // 关键字
  TK_PUNCT,   // 分隔符
  TK_KEYWORD, // 关键字
  TK_NUM,     // 数值字面量
  TK_EOF,     // 文件结束符标记
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

// 局部变量
typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name; // 变量名
  int offset; // 变量相对于fp的偏移量
};

// 函数
typedef struct Function Function;
struct Function {
  Node *body;    // 函数体
  Obj *locals;   // 局部变量
  int stack_size;// 函数栈的大小
};

// 抽象语法树节点
typedef enum {
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_NEG,       // unary -
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_ASSIGN,    // 赋值 =
  ND_RETURN,    // return语句
  ND_BLOCK,     // 块语句 { ... }
  ND_EXPR_STMT, // 表达式语句
  ND_VAR,       // 变量
  ND_NUM,       // 整数
} NodeKind;

// 抽象语法树节点类型
typedef struct Node Node;
struct Node {
  NodeKind kind; // 节点类型
  Node *next;    // 下一个节点的指针
  Node *lhs;     // 运算符左边的节点
  Node *rhs;     // 运算符右边的节点

  // 花括号包含的代码，Block
  Node *body;

  Obj *var;      // 如果 kind == ND_VAR ，则使用这个字段
  int val;       // 如果kind == ND_NUM，则使用这个字段
};

Function *parse(Token *tok);

//
// 代码生成
//

void codegen(Function *prog);