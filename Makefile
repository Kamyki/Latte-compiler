.PHONY: clean

all:
	. ./env && ./install.sh
	. ./env && cargo build --release
	cp target/release/latc_x86_64 ./
	nasm -f elf64 -F dwarf -g ./lib/runtime.s

clean:
	. ./env && cargo clean
	rm latc
