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

PROMPT='[ JavaExtension ]: '

: "${JAVA_VERSION:?JAVA_VERSION must be configured}"
: "${SDKMAN_DIR:?SDKMAN_DIR must be configured}"

install_sdkman() {
  local installer

  if [[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]]; then
    echo_with_prompt "SDKMAN is already installed"
    return
  fi

  echo_with_prompt \
    "Installing SDKMAN at ${SDKMAN_DIR}"

  installer="$(mktemp)"

  curl \
    --fail \
    --location \
    --silent \
    --show-error \
    https://get.sdkman.io \
    --output "${installer}"

  bash "${installer}"

  rm -f -- "${installer}"
}

load_sdkman() {
  local initialization_file
  initialization_file="${SDKMAN_DIR}/bin/sdkman-init.sh"

  if [[ ! -s "${initialization_file}" ]]; then
    echo_with_red_prompt \
      "SDKMAN initialization file is missing: ${initialization_file}"
    return 1
  fi

  # SDKMAN provides `sdk` as a shell function rather than an executable.
  # shellcheck source=/dev/null
  source "${initialization_file}"

  if ! declare -F sdk >/dev/null 2>&1; then
    echo_with_red_prompt \
      "SDKMAN loaded but the sdk function is unavailable"
    return 1
  fi

  # Prevent SDKMAN from prompting during this bootstrap process.
  sdkman_auto_answer=true
}

java_installed_p() {
  [[ -d "${SDKMAN_DIR}/candidates/java/${JAVA_VERSION}" ]]
}

install_java() {
  if java_installed_p; then
    echo_with_prompt \
      "Java ${JAVA_VERSION} is already installed"
  else
    echo_with_prompt \
      "Installing Java ${JAVA_VERSION}"

    sdk install java "${JAVA_VERSION}"
  fi

  sdk default java "${JAVA_VERSION}"
  sdk use java "${JAVA_VERSION}"
}

main() {
  install_sdkman
  load_sdkman
  install_java
}

main "$@"
