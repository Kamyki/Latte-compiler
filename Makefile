.PHONY: clean

all:
	. ./env && ./install.sh
	. ./env && cargo build --release
	cp target/release/latc ./

clean:
	. ./env && cargo clean
	rm latc
