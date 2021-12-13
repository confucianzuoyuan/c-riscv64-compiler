CFLAGS=-std=c11 -g -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

zhizhicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): zhizhicc.h

test: zhizhicc
	./test.sh

clean:
	rm -f zhizhicc *.o *~ tmp*

.PHONY: test clean
