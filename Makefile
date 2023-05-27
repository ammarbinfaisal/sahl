.PHONY: all
all: codegen vm

codegen:
	cargo build --release

vm: vm/*
	gcc vm/* -o sahl -O2 -g -lm -lpthread

aot:
	go build sahl_aot.go

check:
	python3 test.py
