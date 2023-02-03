all:
	$(MAKE) codegen
	$(MAKE) vm
	$(MAKE) aot

codegen:
	cargo build --release

vm:
	gcc -O2 sahl.c -o sahl

aot:
	go build sahl_aot.go
