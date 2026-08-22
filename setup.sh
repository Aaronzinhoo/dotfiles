#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

PROMPT='[ Setup ]: '
DRY_RUN_DEFAULT_VALUE=false
JAVA_DEFAULT_VERSION='21.0.9-tem'
PYTHON_DEFAULT_VERSION='3.12.4'
NODE_DEFAULT_VERSION='24.11.1'
GO_DEFAULT_VERSION='1.25.4'
EMACS_DEFAULT_VERSION='31'
KUBECTL_DEFAULT_VERSION='1.36'

repository_root="$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd
)"

# shellcheck source=./utils.sh
source "${repository_root}/utils.sh"

help() {
  local status="${1:-0}"

  printf '%s\n' \
    'Usage: ./setup.sh -e [work|personal] [options]' \
    '' \
    'Install and configure a workstation.' \
    '' \
    'Options:' \
    '  -h                Display this help message' \
    "  -d                Dry run this script; default: ${DRY_RUN_DEFAULT_VALUE}" \
    '  -e ENVIRONMENT    Environment: work or personal' \
    "  -m VERSION        Emacs version; default: ${EMACS_DEFAULT_VERSION}" \
    "  -g VERSION        Go version; default: ${GO_DEFAULT_VERSION}" \
    "  -j VERSION        Java SDKMAN version; default: ${JAVA_DEFAULT_VERSION}" \
    "  -k VERSION        kubectl minor version; default: ${KUBECTL_DEFAULT_VERSION}" \
    "  -n VERSION        Node version; default: ${NODE_DEFAULT_VERSION}" \
    "  -p VERSION        Python version; default: ${PYTHON_DEFAULT_VERSION}"

  exit "${status}"
}

parse_arguments() {
  local flag

  while getopts ':hde:m:g:j:k:n:p:' flag; do
    case "${flag}" in
      h)
        help 0
        ;;
      d)
        DRY_RUN=true
        ;;
      e)
        case "${OPTARG}" in
          personal|work)
            INSTALL_ENVIRONMENT="${OPTARG}"
            ;;
          *)
            printf \
              'Invalid environment %q; expected work or personal.\n' \
              "${OPTARG}" >&2
            exit 2
            ;;
        esac
        ;;
      m)
        EMACS_VERSION="${OPTARG}"
        ;;
      g)
        GO_VERSION="${OPTARG}"
        ;;
      j)
        JAVA_VERSION="${OPTARG}"
        ;;
      k)
        KUBECTL_VERSION="${OPTARG#v}"
        ;;
      n)
        NODE_VERSION="${OPTARG}"
        ;;
      p)
        PYTHON_VERSION="${OPTARG}"
        ;;
      :)
        printf \
          'Option -%s requires an argument.\n' \
          "${OPTARG}" >&2
        help 2
        ;;
      \?)
        printf \
          'Unknown option: -%s\n' \
          "${OPTARG}" >&2
        help 2
        ;;
    esac
  done

  shift "$((OPTIND - 1))"

  if (( $# > 0 )); then
    printf 'Unexpected argument: %s\n' "$1" >&2
    help 2
  fi
}

set_defaults() {
  export DRY_RUN="${DRY_RUN:-${DRY_RUN_DEFAULT_VALUE}}"
  export EMACS_VERSION="${EMACS_VERSION:-${EMACS_DEFAULT_VERSION}}"
  export GO_VERSION="${GO_VERSION:-${GO_DEFAULT_VERSION}}"
  export INSTALL_ENVIRONMENT="${INSTALL_ENVIRONMENT:-personal}"
  export JAVA_VERSION="${JAVA_VERSION:-${JAVA_DEFAULT_VERSION}}"
  export KUBECTL_VERSION="${KUBECTL_VERSION:-${KUBECTL_DEFAULT_VERSION}}"
  export NODE_VERSION="${NODE_VERSION:-${NODE_DEFAULT_VERSION}}"
  export PYTHON_VERSION="${PYTHON_VERSION:-${PYTHON_DEFAULT_VERSION}}"
}

configure_paths() {
  export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"
  export XDG_CACHE_HOME="${XDG_CACHE_HOME:-${HOME}/.cache}"
  export XDG_DATA_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}"

  export EMACS_INSTALL_DIR="${HOME}/.emacs.d"
  export DEVELOPMENT_DIR_PATH="${HOME}/development"
  export ORG_DIR_PATH="${DEVELOPMENT_DIR_PATH}/org"

  export EDITOR='emacs'

  export CARGO_HOME="${XDG_CONFIG_HOME}/cargo"
  export KREW_ROOT="${XDG_CONFIG_HOME}/krew"
  export GOENV_GOPATH_PREFIX="${XDG_CONFIG_HOME}/go"
  export GOENV_ROOT="${XDG_CONFIG_HOME}/goenv"
  export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
  export PYENV_ROOT="${XDG_CONFIG_HOME}/pyenv"
  export RUSTUP_HOME="${XDG_CONFIG_HOME}/multirust"
  export SDKMAN_DIR="${XDG_CONFIG_HOME}/sdkman"

  # Verify this name against your Zsh plugin. It may be intended to be
  # ZSHZ_DATA or _Z_DATA rather than ZHSZ_DATA.
  export ZHSZ_DATA="${XDG_CONFIG_HOME}/z"
}

create_directories() {
  echo_with_prompt 'Creating required directories'

  mkdir -p \
    "${GOENV_GOPATH_PREFIX}" \
    "${DEVELOPMENT_DIR_PATH}" \
    "${ORG_DIR_PATH}" \
    "${ORG_DIR_PATH}/notebook" \
    "${ORG_DIR_PATH}/references" \
    "${ORG_DIR_PATH}/work" \
    "${HOME}/.local/bin" \
    "${HOME}/.ssh/github" \
    "${XDG_CONFIG_HOME}" \
    "${XDG_CACHE_HOME}" \
    "${XDG_DATA_HOME}" \
    "${XDG_CONFIG_HOME}/emacs/backups" \
    "${NVM_DIR}"
}

print_configuration() {
  echo_with_prompt "EMACS_VERSION=${EMACS_VERSION}"
  echo_with_prompt "DRY_RUN=${DRY_RUN}"
  echo_with_prompt "GO_VERSION=${GO_VERSION}"
  echo_with_prompt "INSTALL_ENVIRONMENT=${INSTALL_ENVIRONMENT}"
  echo_with_prompt "JAVA_VERSION=${JAVA_VERSION}"
  echo_with_prompt "KUBECTL_VERSION=${KUBECTL_VERSION}"
  echo_with_prompt "NODE_VERSION=${NODE_VERSION}"
  echo_with_prompt "PYTHON_VERSION=${PYTHON_VERSION}"
}

main() {
  parse_arguments "$@"
  set_defaults
  configure_paths

  echo_with_prompt 'Initializing setup'
  print_configuration

  if [[ "${DRY_RUN}" == false ]]; then
    create_directories
  else
    echo_with_prompt "Skipping directory creation because DRY_RUN=${DRY_RUN}"
  fi

  echo_with_prompt 'Installing packages and bootstrapping'

  if [[ "${DRY_RUN}" == false ]]; then
    "${repository_root}/bootstrap.sh"
  else
    echo_with_prompt "Skipping ${repository_root}/bootstrap.sh because DRY_RUN=${DRY_RUN}"
  fi

  echo_with_prompt 'Finished! Enjoy!'
}

main "$@"
