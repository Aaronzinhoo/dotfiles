#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# shellcheck source=utils.sh
source "${REPOSITORY_ROOT}/utils.sh"

PROMPT='[ Bootstrap ]: '

link_path() {
  local source="${1:?missing source}"
  local destination="${2:?missing destination}"

  if [[ ! -e "${source}" ]]; then
    echo_with_prompt "Source does not exist: ${source}"
    return 1
  fi

  if [[ -L "${destination}" ]]; then
    if [[ "$(readlink "${destination}")" == "${source}" ]]; then
      echo_with_prompt "Already linked: ${destination}"
      return 0
    fi

    echo_with_prompt "Updating symlink: ${destination}"
    ln -sfn "${source}" "${destination}"
    return 0
  fi

  if [[ -e "${destination}" ]]; then
    echo_with_prompt \
      "Skipping ${destination}: a non-symlink file already exists."
    return 0
  fi

  echo_with_prompt "Linking ${source} -> ${destination}"
  ln -s "${source}" "${destination}"
}

link_configuration_directory() {
  local source_directory="${1:?missing source directory}"
  local destination_directory="${2:?missing destination directory}"
  local source
  local destination

  [[ -d "${source_directory}" ]] || return 0

  mkdir -p "${destination_directory}"

  while IFS= read -r -d '' source; do
    destination="${destination_directory}/$(basename "${source}")"

    link_path \
      "${source}" \
      "${destination}"
  done < <(
    find "${source_directory}" \
      -mindepth 1 \
      -maxdepth 1 \
      -print0
  )
}

link_configuration() {
  : "${EMACS_INSTALL_DIR:?EMACS_INSTALL_DIR must be configured}"
  : "${XDG_CONFIG_HOME:?XDG_CONFIG_HOME must be configured}"

  # Link the complete Emacs configuration directory.
  link_path \
    "${REPOSITORY_ROOT}/emacs" \
    "${EMACS_INSTALL_DIR}"

  # Files stored under configs/home go directly into $HOME.
  link_configuration_directory \
    "${REPOSITORY_ROOT}/configs/home" \
    "${HOME}"

  # Directories stored under configs/xdg go into $XDG_CONFIG_HOME.
  link_configuration_directory \
    "${REPOSITORY_ROOT}/configs/xdg" \
    "${XDG_CONFIG_HOME}"
}

install_macos_packages() {
  echo_with_prompt "Detected macOS"
  echo_with_prompt \
    "This utility can install the declared Homebrew dependencies."

  if confirm "Proceed with Homebrew installation?"; then
    echo_with_prompt "Installing Homebrew dependencies"

    "${REPOSITORY_ROOT}/package_managers/brew_packages.sh"
  else
    echo_with_prompt "Homebrew installation cancelled"
  fi
}

install_apt_packages() {
  if ! command -v apt-get >/dev/null 2>&1; then
    echo_with_prompt "This Linux distribution does not provide apt-get; skipping packages."
    return 0
  fi

  echo_with_prompt "Detected an apt-based Linux distribution"
  echo_with_prompt "This utility can install the declared apt dependencies."

  if confirm "Proceed with apt installation?"; then
    echo_with_prompt "Installing apt dependencies"

    # Let the package script use sudo only for commands that require it.
    "${REPOSITORY_ROOT}/package_managers/apt_packages.sh"
  else
    echo_with_prompt "Apt installation cancelled"
  fi
}

install_packages() {
  case "$(uname -s)" in
    Darwin)
      install_macos_packages
      ;;
    Linux)
      install_apt_packages
      ;;
    *)
      echo_with_prompt "No compatible package manager was detected; skipping packages."
      ;;
  esac
}

running_under_wsl() {
  [[ -n "${WSL_DISTRO_NAME:-}" ]] ||
    [[ -n "${WSL_INTEROP:-}" ]] ||
    grep --quiet --ignore-case microsoft /proc/version 2>/dev/null
}

run_bootstrap_extension() {
  local bootstrap_file="${1:?missing bootstrap file}"

  if [[ ! -f "${bootstrap_file}" ]]; then
    echo_with_prompt "Bootstrap extension does not exist: ${bootstrap_file}"
    return 1
  fi

  echo_with_prompt "Applying $(basename "${bootstrap_file}")"
  "${bootstrap_file}"
}

run_bootstrap_extensions() {
  local bootstrap_directory
  local bootstrap_file
  local bootstrap_name
  local zsh_bootstrap

  bootstrap_directory="${REPOSITORY_ROOT}/bootstrap_extensions"
  zsh_bootstrap="${bootstrap_directory}/zsh_bootstrap.sh"

  # Configure the shell first if the extension exists.
  if [[ -f "${zsh_bootstrap}" ]]; then
    run_bootstrap_extension "${zsh_bootstrap}"
  fi

  while IFS= read -r -d '' bootstrap_file; do
    bootstrap_name="$(basename "${bootstrap_file}")"
    case "${bootstrap_name}" in
      # skip zsh bootstrap since we do it above
      zsh_bootstrap.sh)
         continue
         ;;
      wsl_bootstrap.sh)
         if ! running_under_wsl; then
           echo_with_prompt \
             "Skipping ${bootstrap_name}: this is not WSL."
           continue
         fi
         ;;
    esac
    run_bootstrap_extension "${bootstrap_file}"
  done < <(find "${bootstrap_directory}" \
                -mindepth 1 \
                -maxdepth 1 \
                -type f \
                -name '*_bootstrap.sh' \
                -print0 |
             sort -z)
}

main() {
  echo_with_prompt "Running Bootstrap."
  if [[ "$DRY_RUN" == false ]]; then
    execute_func_with_prompt link_configuration "Attempting to link configuration files"
    install_packages
    run_bootstrap_extensions
  else
    echo_with_prompt "Skipping linking, installing, and running bootstrap and friends since DRY_RUN=${DRY_RUN}."
  fi
  echo_with_prompt "Bootstrap completed."
}

main "$@"
