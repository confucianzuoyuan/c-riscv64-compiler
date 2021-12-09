#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./zhizhicc "$input" > tmp.s || exit
  riscv64-unknown-elf-gcc -static -o tmp tmp.s
  qemu-riscv64 ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => 预期值：$expected, 实际值：$actual"
    exit 1
  fi
}

assert 0 0
assert 42 42

echo OK
