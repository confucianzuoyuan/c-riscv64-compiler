CFLAGS=-std=c11 -g -fno-common

zhizhicc: main.o
	$(CC) -o zhizhicc main.o $(LDFLAGS)

test: zhizhicc
	./test.sh

clean:
	rm -f zhizhicc *.o *~ tmp*

.PHONY: test clean
