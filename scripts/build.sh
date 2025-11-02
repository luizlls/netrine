#!/bin/bash

build() {
  cargo build -p wasm --profile wasm --target wasm32-unknown-unknown --release
}

move() {
  mv target/wasm32-unknown-unknown/release/wasm.wasm app/wasm/wasm.wasm
}

optmize() {
  wasm-opt -Oz app/wasm/wasm.wasm -o app/wasm/wasm.wasm --enable-bulk-memory-opt
}

build && move && optmize
