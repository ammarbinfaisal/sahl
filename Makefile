.PHONY: vm

all: codegen vm runtime

codegen:
	cargo build --release

runtime: runtime/*
	cd runtime && cargo build --release

vm: vm/*
	gcc vm/* -o sahl -O2 -g -lm -lpthread

check:
	python3 test.py
