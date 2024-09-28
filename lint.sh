#!/usr/bin/env sh
cargo clippy --all -- -Dwarnings
exit $?
