#include "zhizhicc.h"

static char *opt_o;

static char *input_path;

static void usage(int status) {
  fprintf(stderr, "zhizhicc [ -o <path> ] <file>\n");
  exit(status);
}

static void parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--help"))
      usage(0);

    if (!strcmp(argv[i], "-o")) {
      if (!argv[++i])
        usage(1);
      opt_o = argv[i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      opt_o = argv[i] + 2;
      continue;
    }

    if (argv[i][0] == '-' && argv[i][1] != '\0')
      error("未知参数: %s", argv[i]);

    input_path = argv[i];
  }

  if (!input_path)
    error("没有输入文件");
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0)
    return stdout;

  FILE *out = fopen(path, "w");
  if (!out)
    error("无法打开输出文件: %s: %s", path, strerror(errno));
  return out;
}

int main(int argc, char **argv) {
  parse_args(argc, argv);

  // 词法分析和语法分析
  Token *tok = tokenize_file(input_path);
  Obj *prog = parse(tok);

  // 遍历语法树，输入汇编代码
  FILE *out = open_file(opt_o);
  codegen(prog, out);
  return 0;
}
