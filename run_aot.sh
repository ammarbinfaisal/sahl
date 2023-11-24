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

function check {
    if [ $? -ne 0 ]; then
        echo "error: failed to compile $1"
        exit 1
    fi
}

# $rt=./runtime/target/release/libruntime.so
rt=./rt.c

./frontend/target/release/sahl $1 -n 2>./exe.ll

check $1

clang -opaque-pointer -O2 -c ./exe.ll -o ./exe.o || exit 1
clang -O2 -c $rt -o ./rt.o || exit 1
clang -O2 ./exe.o ./rt.o ./libs/libgc.a -o ./exe

check $1

./exe