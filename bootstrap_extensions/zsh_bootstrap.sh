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

PROMPT='[ ZshExtension ]: '

oh_my_zsh_directory="${HOME}/.oh-my-zsh"
zsh_custom_directory="${ZSH_CUSTOM:-${oh_my_zsh_directory}/custom}"
custom_plugins_directory="${zsh_custom_directory}/plugins"
custom_themes_directory="${zsh_custom_directory}/themes"

require_command() {
  local command_name="${1:?Command name is required}"

  if ! command -v "${command_name}" >/dev/null 2>&1; then
    echo_with_red_prompt \
      "${command_name} is required but is not installed"
    return 1
  fi
}

clone_if_missing() {
  local repository="${1:?Repository URL is required}"
  local destination="${2:?Destination is required}"

  if [[ -d "${destination}/.git" ]]; then
    echo_with_prompt \
      "$(basename "${destination}") is already installed"
    return
  fi

  if [[ -e "${destination}" ]]; then
    echo_with_red_prompt \
      "${destination} exists but is not a Git repository"
    return 1
  fi

  echo_with_prompt "Cloning ${repository}"

  git clone \
    --depth 1 \
    "${repository}" \
    "${destination}"
}

install_oh_my_zsh() {
  if [[ -d "${oh_my_zsh_directory}/.git" ]]; then
    echo_with_prompt "Oh My Zsh is already installed"
    return
  fi

  if [[ -e "${oh_my_zsh_directory}" ]]; then
    echo_with_red_prompt \
      "${oh_my_zsh_directory} exists but is not a Git repository"
    return 1
  fi

  echo_with_prompt "Installing Oh My Zsh"

  git clone \
    --depth 1 \
    https://github.com/ohmyzsh/ohmyzsh.git \
    "${oh_my_zsh_directory}"
}

install_theme() {
  mkdir -p "${custom_themes_directory}"

  clone_if_missing \
    https://github.com/romkatv/powerlevel10k.git \
    "${custom_themes_directory}/powerlevel10k"
}

install_plugins() {
  mkdir -p "${custom_plugins_directory}"

  clone_if_missing \
    https://github.com/zsh-users/zsh-autosuggestions.git \
    "${custom_plugins_directory}/zsh-autosuggestions"

  clone_if_missing \
    https://github.com/zsh-users/zsh-completions.git \
    "${custom_plugins_directory}/zsh-completions"

  clone_if_missing \
    https://github.com/zsh-users/zsh-syntax-highlighting.git \
    "${custom_plugins_directory}/zsh-syntax-highlighting"

  clone_if_missing \
    https://github.com/agkozak/zsh-z.git \
    "${custom_plugins_directory}/zsh-z"

  clone_if_missing \
    https://github.com/TamCore/autoupdate-oh-my-zsh-plugins.git \
    "${custom_plugins_directory}/autoupdate"

  clone_if_missing \
    https://github.com/lukechilds/zsh-nvm.git \
    "${custom_plugins_directory}/zsh-nvm"

  clone_if_missing \
    https://github.com/lukechilds/zsh-better-npm-completion.git \
    "${custom_plugins_directory}/zsh-better-npm-completion"

  clone_if_missing \
    https://github.com/johanhaleby/kubetail.git \
    "${custom_plugins_directory}/kubetail"
}

configure_default_shell() {
  local zsh_path
  local current_shell

  zsh_path="$(command -v zsh)"
  current_shell="${SHELL:-}"

  if [[ "${current_shell}" == "${zsh_path}" ]]; then
    echo_with_prompt "Zsh is already the default shell"
    return
  fi

  echo_with_prompt "Changing the default shell to ${zsh_path}"
  chsh -s "${zsh_path}"
}

main() {
  require_command git
  require_command zsh

  install_oh_my_zsh
  install_theme
  install_plugins

  if [[ "${DRY_RUN:-false}" != true ]]; then
    configure_default_shell
  fi

  echo_with_green_prompt "Completed Zsh bootstrapping"
}

main "$@"
