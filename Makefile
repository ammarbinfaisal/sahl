all:
	$(MAKE) codegen
	$(MAKE) vm

codegen:
	cargo build --release

vm:
	gcc -O2 sahl.c -o sahl
