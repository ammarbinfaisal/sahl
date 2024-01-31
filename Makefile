.PHONY: vm

all: codegen vm

codegen:
	cd frontend && cargo build --release

vm: vm/*
	gcc vm/* libs/libtreadmill.a  -o sahl -Iinclude -O2 -g -lm -lpthread

check:
	python3 test.py
