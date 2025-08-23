.PHONY: build
build:
	cargo build --all

.PHONY: release
release:
	RUSTFLAGS="-D warnings -C target-cpu=native" cargo build --bin hnc --release

.PHONY: maxperf
maxperf:
	RUSTFLAGS="-D warnings -C target-cpu=native" cargo build --bin hnc --profile maxperf

.PHONY: run
run:
	cargo run

.PHONY: test
test:
	@command -v cargo-nextest >/dev/null 2>&1 || cargo install cargo-nextest --locked
	cargo nextest run --workspace --bins --lib --tests

.PHONY: test-cargo
test-cargo:
	cargo test --workspace --bins --lib --tests

.PHONY: test-doc
test-doc:
	cargo test --doc --workspace

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
	RUSTDOCFLAGS="--show-type-layout --generate-link-to-definition --enable-index-page -D warnings -Z unstable-options" \
	cargo +nightly doc --workspace --all-features --no-deps --document-private-items

.PHONY: udeps
udeps:
	cargo install cargo-machete --locked && cargo-machete --with-metadata

.PHONY: pre-release
pre-release:
	mdbook test
	mdbook-linkcheck --standalone
	make fmt
	make taplo
	make clippy
	make test
	make test-doc
	make deny-check