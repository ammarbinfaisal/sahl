all:
	$(MAKE) codegen
	$(MAKE) vm

codegen:
	cargo build --release

vm:
	gcc sahl.c -o sahl
