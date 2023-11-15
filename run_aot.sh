#!/bin/bash

#  check if args are passed
if [ $# -eq 0 ]; then
    echo "usage: $0 <file>"
    exit 1
fi

#  check if file exists
if [ ! -f $1 ]; then
    echo "error: file $1 does not exist"
    exit 1
fi

./frontend/target/release/sahl $1 -n 2>./exe.ll && clang -opaque-pointer -O3 ./exe.ll ./runtime/target/release/libruntime.so -o ./exe && ./exe
