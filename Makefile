.PHONY: vm

all: codegen vm rts

codegen:
	cd frontend && cargo build --release

rts:
	cd runtime && cargo build --release

vm: vm/*
	gcc vm/* -o sahl -O2 -g -lm -lpthread

check:
	python3 test.py
