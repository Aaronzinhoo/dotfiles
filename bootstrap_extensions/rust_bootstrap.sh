#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

script_directory="$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd
)"

# shellcheck source=../utils.sh
source "${REPOSITORY_ROOT}/utils.sh"

PROMPT='[ RustExtension ]: '

RUST_TOOLCHAIN="${RUST_TOOLCHAIN:-stable}"
CARGO_TOOLS=(
  'cargo-audit@0.21.2'
  'cargo-edit@0.13.6'
  'emacs-lsp-booster@0.2.1'
)

: "${CARGO_HOME:?CARGO_HOME must be configured}"
: "${RUSTUP_HOME:?RUSTUP_HOME must be configured}"

install_rustup() {
  local installer

  if [[ -x "${CARGO_HOME}/bin/rustup" ]]; then
    echo_with_prompt "rustup is already installed"
    return
  fi

  echo_with_prompt "Installing rustup"

  mkdir -p \
    "${CARGO_HOME}" \
    "${RUSTUP_HOME}"

  installer="$(mktemp)"

  curl --proto '=https' --tlsv1.2 --fail --location --silent --show-error https://sh.rustup.rs --output "${installer}"

  # Shell initialization is managed by the dotfiles configuration.
  sh "${installer}" -y --no-modify-path --default-toolchain none

  rm -f "${installer}"
}

load_rustup() {
  # Make rustup and its proxy commands available to this bootstrap.
  export PATH="${CARGO_HOME}/bin:${PATH}"

  if ! command -v rustup >/dev/null 2>&1; then
    echo_with_red_prompt \
      "rustup is installed but unavailable on PATH"
    return 1
  fi
}

install_rust_toolchain() {
  echo_with_prompt \
    "Installing Rust toolchain ${RUST_TOOLCHAIN}"

  rustup toolchain install \
    "${RUST_TOOLCHAIN}" \
    --profile minimal \
    --component rustfmt \
    --component clippy \
    --component rust-src \
    --component rust-analyzer

  rustup default "${RUST_TOOLCHAIN}"

  echo_with_green_prompt "Using $(rustc --version)"
}

install_cargo_tools() {
  local tool

  echo_with_prompt "Installing Cargo tools"

  for tool in "${CARGO_TOOLS[@]}"; do
    echo_with_prompt "Installing ${tool}"

    cargo install \
      --locked \
      "${tool}"
  done

  echo_with_green_prompt \
    "Finished installing Cargo tools"
}

main() {
  install_rustup
  load_rustup
  install_rust_toolchain
  install_cargo_tools
}

main "$@"
