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

PROMPT='[ NodeExtension ]: '

NVM_VERSION="${NVM_VERSION:-v0.40.7}"

: "${DRY_RUN:?DRY_RUN must be configured}"
: "${NVM_DIR:?NVM_DIR must be configured}"
: "${NODE_VERSION:?NODE_VERSION must be configured}"

PACKAGES=(
  '@angular/language-server'
  '@ansible/ansible-language-server'
  '@asyncapi/cli'
  '@redocly/cli'
  'bash-language-server'
  'dockerfile-language-server-nodejs'
  'prettier'
  'pyright'
  'vscode-langservers-extracted'
  'yaml-language-server'
)

install_nvm() {
  local installer

  if [[ -s "${NVM_DIR}/nvm.sh" ]]; then
    echo_with_prompt "NVM is already installed"
    return
  fi

  if [[ -e "${NVM_DIR}" ]] &&
     [[ ! -d "${NVM_DIR}" ]]; then
    echo_with_red_prompt \
      "${NVM_DIR} exists but is not a directory"
    return 1
  fi

  echo_with_prompt "Installing NVM ${NVM_VERSION} at ${NVM_DIR}"

  mkdir -p "${NVM_DIR}"
  installer="$(mktemp)"

  curl \
    --fail \
    --location \
    --silent \
    --show-error \
    "https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_VERSION}/install.sh" \
    --output "${installer}"

  # The dotfiles repository manages shell initialization itself.
  PROFILE=/dev/null bash "${installer}"

  rm -f "${installer}"

  # load nvm
  # shellcheck source=/dev/null
  source "${NVM_DIR}/nvm.sh"
}

configure_default_packages() {
  printf '%s\n' \
    "${PACKAGES[@]}" \
    >"${NVM_DIR}/default-packages"
}

install_node() {
  echo_with_prompt "Installing Node ${NODE_VERSION}"

  nvm install "${NODE_VERSION}"
  nvm alias default "${NODE_VERSION}"
  nvm use "${NODE_VERSION}"

  echo_with_green_prompt \
    "Using $(node --version) with npm $(npm --version)"
}

install_global_packages() {
  echo_with_prompt "Installing global Node tools"

  npm install \
    --global \
    "${PACKAGES[@]}"

  echo_with_green_prompt "Finished installing global Node tools"
}

main() {
  echo_with_green_prompt "Starting Node Bootstrapping"
  if [[ "${DRY_RUN:-false}" != true ]]; then
    install_nvm
    configure_default_packages
    install_node
    install_global_packages
  fi
  echo_with_green_prompt "Finished Node Bootstrapping"
}

main "$@"
