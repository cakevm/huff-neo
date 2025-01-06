.PHONY: build run test clean fmt fmt-check clippy taplo taplo-check deny-check docs

build:
	cargo build --all

release:
	cargo build --release

run:
	cargo run

test:
	cargo test --workspace --bins --lib --tests

clean:
	cargo clean

fmt:
	cargo fmt --all

fmt-check:
	cargo fmt --all --check

clippy:
	cargo clippy --all --all-features --lib --tests --benches -- -D warnings

taplo:
	taplo format

taplo-check:
	taplo format --check

deny-check:
	cargo deny --all-features check

doc:
	cargo doc --workspace --all-features --no-deps