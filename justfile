default: ci

check-fmt:
    cargo fmt --all --check

format:
    cargo fmt --all

alias fmt := format

test-all:
    cargo test --workspace

alias ta := test-all

test package:
    cargo test --package {{package}}

alias t := test

build-all args:
    cargo build --workspace {{args}}

alias ba := build-all

build package args:
    cargo build --package {{package}} {{args}}

alias b := build

lint:
    cargo clippy --all -- -Dwarnings

alias check := lint

ci: check-fmt lint test-all
