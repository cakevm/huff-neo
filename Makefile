.PHONY: build
build:
	cargo build --all

.PHONY: release
release:
	RUSTFLAGS="-D warnings -C target-cpu=native" cargo build --bin huff-neo --release

.PHONY: maxperf
maxperf:
	RUSTFLAGS="-D warnings -C target-cpu=native" cargo build --bin huff-neo --profile maxperf

.PHONY: run
run:
	cargo run

.PHONY: test
test:
	cargo test --workspace --bins --lib --tests

.PHONY: clean
clean:
	cargo clean

.PHONY: fmt
fmt:
	cargo fmt --all

.PHONY: fmt-check
fmt-check:
	cargo fmt --all --check

.PHONY: clippy
clippy:
	cargo clippy --all --all-features --lib --tests --benches -- -D warnings

.PHONY: taplo
taplo:
	taplo format

.PHONY: taplo-check
taplo-check:
	taplo format --check

.PHONY: deny-check
deny-check:
	cargo deny --all-features check

.PHONY: doc
doc:
	cargo doc --workspace --all-features --no-deps

.PHONY: pre-release
pre-release:
	make fmt
	make clippy
	make test
	make taplo-check
	make deny-check