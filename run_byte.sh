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

./target/release/sahl $1 -c && ./sahl exe.bin
