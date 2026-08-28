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

PROMPT='[ GoExtension ]: '

# ensure env vars set
: "${GO_VERSION:?GO_VERSION must be configured}"
: "${GOENV_ROOT:?GOENV_ROOT must be configured}"

configure_goenv() {
  export PATH="${GOENV_ROOT}/bin:${PATH}"

  if ! command -v goenv >/dev/null 2>&1; then
    echo_with_red_prompt \
      "goenv is not installed. Install it through the platform package setup."
    return 1
  fi

  eval "$(goenv init - bash)"
}

install_go() {
  if goenv versions --bare |
     command grep -Fxq "${GO_VERSION}"; then
    echo_with_prompt "Go ${GO_VERSION} is already installed"
  else
    echo_with_prompt "Installing Go ${GO_VERSION}"
    goenv install "${GO_VERSION}"
  fi

  goenv global "${GO_VERSION}"
  goenv rehash

  # Make the selected Go installation immediately available to this script.
  eval "$(goenv init - bash)"

  echo_with_green_prompt \
    "Using $(go version)"
}

install_go_tools() {
  local tools=(
    'golang.org/x/tools/gopls@latest'
    'github.com/lighttiger2505/sqls@latest'
    'golang.org/x/vuln/cmd/govulncheck@latest'
    'github.com/CycloneDX/cyclonedx-gomod/cmd/cyclonedx-gomod@latest'
  )
  local tool

  echo_with_prompt "Installing Go tools"

  for tool in "${tools[@]}"; do
    echo_with_prompt "Installing ${tool}"
    go install "${tool}"
  done

  echo_with_green_prompt "Finished installing Go tools"
}

main() {
  configure_goenv
  if [[ "${DRY_RUN:-false}" != true ]]; then
    install_go
    install_go_tools
  fi
}

main "$@"
