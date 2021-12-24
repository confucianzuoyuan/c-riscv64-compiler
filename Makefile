CFLAGS=-std=c11 -g -fno-common

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

zhizhicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): zhizhicc.h

test/%.exe: zhizhicc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./zhizhicc -o test/$*.s -
	riscv64-unknown-elf-gcc -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; qemu-riscv64 ./$$i || exit 1; echo; done
	test/driver.sh

clean:
	rm -rf zhizhicc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean
