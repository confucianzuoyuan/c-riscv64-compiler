#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

typedef struct Node Node;
typedef struct Type Type;
typedef struct Member Member;

//
// strings.c
//

char *format(char *fmt, ...);

//
// 词法分析器
//

typedef enum {
  TK_IDENT,   // 关键字
  TK_PUNCT,   // 分隔符
  TK_KEYWORD, // 关键字
  TK_STR,     // 字符串字面量
  TK_NUM,     // 数值字面量
  TK_EOF,     // 文件结束符标记
} TokenKind;

// 标记类型
typedef struct Token Token;
struct Token {
  TokenKind kind; // 标记类型
  Token *next;    // 下一个标记的指针
  int64_t val;    // 如果标记是TK_NUM，它的值
  char *loc;      // 标记的位置
  int len;        // 标记的长度
  Type *ty;       // 如果是TK_STR标记，则使用这个字段
  char *str;      // 字符串字面量的内容，包括最后的'\0'终结字符

  int line_no;    // 行号
};

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize_file(char *filename);

#define unreachable() \
  error("内部错误，位置 %s:%d", __FILE__, __LINE__)

//
// 语法分析器
//

// 局部变量
typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;    // 变量名
  Type *ty;      // 类型
  bool is_local; // 局部变量还是全局变量/函数

  int offset;    // 局部变量相对于fp的偏移量

  // 全局变量还是函数
  bool is_function;
  bool is_definition;
  bool is_static;

  // 全局变量
  char *init_data;

  // 函数
  Obj *params;
  Node *body;
  Obj *locals;
  int stack_size;
};

// 抽象语法树节点
typedef enum {
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_NEG,       // unary -
  ND_MOD,       // %
  ND_BITAND,    // &
  ND_BITOR,     // |
  ND_BITXOR,    // ^
  ND_LOGAND,    // &&
  ND_LOGOR,     // ||
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_ASSIGN,    // 赋值 =
  ND_COMMA,     // ,
  ND_MEMBER,    // . (结构体成员的访问操作符)
  ND_ADDR,      // & 取地址运算符
  ND_DEREF,     // * 解引用运算符
  ND_NOT,       // !
  ND_BITNOT,    // ~
  ND_RETURN,    // return语句
  ND_IF,        // if语句
  ND_FOR,       // for语句或者while语句
  ND_SWITCH,    // "switch"
  ND_CASE,      // "case"
  ND_BLOCK,     // 块语句 { ... }
  ND_GOTO,      // "goto"
  ND_LABEL,     // 带标签的语句
  ND_FUNCALL,   // 函数调用
  ND_EXPR_STMT, // 表达式语句
  ND_STMT_EXPR, // 语句表达式
  ND_VAR,       // 变量
  ND_NUM,       // 整数
  ND_CAST,      // 强制类型转换
} NodeKind;

// 抽象语法树节点类型
struct Node {
  NodeKind kind; // 节点类型
  Node *next;    // 下一个节点的指针
  Type *ty;      // 类型，例如 int 或者 指向 int 的指针
  Token *tok;    // 语法树节点的tok代表

  Node *lhs;     // 运算符左边的节点
  Node *rhs;     // 运算符右边的节点

  // "if" 语句或者 "for" 语句
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // "break"和"continue"标签
  char *brk_label;
  char *cont_label;

  // 花括号包含的代码，Block或者语句表达式
  Node *body;

  // 结构提成员的访问
  Member *member;

  // 函数调用
  char *funcname;
  Type *func_ty;
  Node *args;

  // Goto或者带标签的语句
  char *label;
  char *unique_label;
  Node *goto_next;

  // Switch-cases
  Node *case_next;
  Node *default_case;

  // Variable
  Obj *var;

  // Numeric literal
  int64_t val;
};

Node *new_cast(Node *expr, Type *ty);
Obj *parse(Token *tok);

//
// 类型
//

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_ENUM,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
  TY_STRUCT,
  TY_UNION,
} TypeKind;

struct Type {
  TypeKind kind;
  int size;       // sizeof()的值
  int align;      // 内存对齐

  // 指针
  Type *base;

  // 声明
  Token *name;

  // 数组的长度
  int array_len;

  // 结构体
  Member *members;

  // 函数类型就是返回值的类型
  Type *return_ty;
  Type *params;
  Type *next;
};

// 结构体成员
struct Member {
  Member *next;
  Type *ty;
  Token *tok; // 供错误信息使用
  Token *name;
  int offset;
};

extern Type *ty_void;
extern Type *ty_bool;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

bool is_integer(Type *ty);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int size);
Type *enum_type(void);
Type *struct_type(void);
void add_type(Node *node);

//
// 代码生成
//

void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);