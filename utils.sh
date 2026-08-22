#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# This file is intended to be sourced by other Bash scripts.

# -------------------------------------------------------------------
# Colors
# -------------------------------------------------------------------

readonly COLOR_BLUE=$'\033[34m'
readonly COLOR_GREEN=$'\033[32m'
readonly COLOR_YELLOW=$'\033[33m'
readonly COLOR_RED=$'\033[31m'
readonly COLOR_RESET=$'\033[0m'

supports_color() {
  if [[ -n "${NO_COLOR:-}" ]]; then
    return 1
  fi
  if [[ "${FORCE_COLOR:-false}" == 'true' ]]; then
    return 0
  fi
  [[ -t 1 ]] &&
    [[ "${TERM:-dumb}" != 'dumb' ]]
}

print_color() {
  local color="${1:?missing color}"
  shift

  if supports_color; then
    printf '%s%s%s%s\n' \
           "$(date +'%d-%m-%Y %T ')" \
           "${color}" \
           "$*" \
           "${COLOR_RESET}"
  else
    printf '%s%s\n'\
           "$(date +'%d-%m-%Y %T ')" \
           "$*"
  fi
}

print_color_no_newline() {
  local color="${1:?missing color}"
  shift

  if supports_color; then
    printf '%s%s%s%s' \
           "$(date +'%d-%m-%Y %T ')" \
           "${color}" \
           "$*" \
           "${COLOR_RESET}"
  else
    printf '%s%s' \
           "$(date +'%d-%m-%Y %T ')" \
           "$*"
  fi
}

color_blue() {
  print_color "${COLOR_BLUE}" "$@"
}


color_green() {
  print_color "${COLOR_GREEN}" "$@"
}


color_yellow() {
  print_color "${COLOR_YELLOW}" "$@"
}


color_red() {
  print_color "${COLOR_RED}" "$@"
}

# -------------------------------------------------------------------
# Prompt output
# -------------------------------------------------------------------

current_prompt()
{
  printf '%s' \
    "${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
}


echo_with_no_newline_prompt()
{
  print_color_no_newline \
    "${COLOR_BLUE}" \
    "$(current_prompt) $*"
}


echo_with_prompt()
{
  color_blue \
    "$(current_prompt) $*"
}


echo_with_green_prompt()
{
  color_green \
    "$(current_prompt) $*"
}


echo_with_yellow_prompt()
{
  color_yellow \
    "$(current_prompt) $*"
}


echo_with_red_prompt()
{
  color_red \
    "$(current_prompt) $*"
}


confirm() {
  local question="${1:-Proceed?}"
  local response

  echo_with_no_newline_prompt "${question} (y/n): "

  if ! IFS= read -r response; then
    # Ensure subsequent output does not remain on the prompt line.
    printf '\n'
    return 1
  fi

  case "${response}" in
    y|Y|yes|Yes|YES)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

# -------------------------------------------------------------------
# Command execution
# -------------------------------------------------------------------

command_exists()
{
  local command_name="${1:?missing command name}"

  command -v "${command_name}" >/dev/null 2>&1
}

execute_func_with_prompt()
{
  local operation="${1:?missing function or command}"
  local description="${2:?missing operation description}"
  local status

  shift 2

  if ! command_exists "${operation}"; then
    echo_with_red_prompt \
      "Unknown function or command: ${operation}"

    return 2
  fi

  echo_with_prompt "${description}"

  if ! confirm 'Proceed?'; then
    echo_with_yellow_prompt \
      "${operation} execution cancelled"

    return 1
  fi

  # Running the command inside `if' prevents `errexit' from
  # terminating the script before we can handle its status.
  if "${operation}" "$@"; then
    echo_with_green_prompt \
      "${operation} execution complete"

    return 0
  else
    status=$?

    echo_with_red_prompt \
      "${operation} failed with status ${status}"

    return 2
  fi
}


# -------------------------------------------------------------------
# Bootstrap checks
# -------------------------------------------------------------------

install_bootstrap_check()
{
  local check_function="${1:?missing bootstrap check function}"
  local extension_name="${2:?missing extension name}"

  if ! command_exists "${check_function}"; then
    echo_with_red_prompt \
      "Unknown bootstrap check function: ${check_function}"

    return 2
  fi

  # A successful check means the extension appears to be installed.
  if "${check_function}"; then
    echo_with_prompt \
      "Bootstrapping for ${extension_name} appears to be complete."

    if ! confirm \
      'Do you wish to run the bootstrap process again?'; then
      echo_with_yellow_prompt \
        "Skipping ${extension_name} bootstrap"

      return 1
    fi
  fi

  return 0
}
