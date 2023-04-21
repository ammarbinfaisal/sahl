all:
	$(MAKE) codegen
	$(MAKE) vm
	$(MAKE) aot

codegen:
	cargo build --release

vm:
	gcc vm/* -o sahl -O2 -g -lm -lpthread

aot:
	go build sahl_aot.go
