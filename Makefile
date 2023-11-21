.PHONY: vm

all: codegen vm rts gc

codegen:
	cd frontend && cargo build --release

rts:
	cd runtime && cargo build --release

vm: vm/*
	gcc vm/* -o sahl -O2 -g -lm -lpthread

gc:
	cd bdwgc && ./autogen.sh && ./configure --enable-threads=posix --enable-large-config --enable-parallel-mark --enable-static --disable-shared --disable-gcc-warnings && make 

check:
	python3 test.py
