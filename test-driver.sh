#!/bin/sh
tmp=`mktemp -d /tmp/zhizhicc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
./zhizhicc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
./zhizhicc --help 2>&1 | grep -q zhizhicc
check --help

echo OK
