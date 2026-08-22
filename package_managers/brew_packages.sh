#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

script_directory="$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd
)"

repository_root="$(
  cd -- "${script_directory}/.."
  pwd
)"

# shellcheck source=../utils.sh
source "${repository_root}/utils.sh"

export PROMPT='[ BrewInstaller ]: '

brewfile="${script_directory}/Brewfile"


# -------------------------------------------------------------------
# Xcode Command Line Tools
# -------------------------------------------------------------------

if ! xcode-select --print-path >/dev/null 2>&1; then
  echo_with_prompt "Installing Xcode Command Line Tools"

  xcode-select --install

  echo_with_prompt \
    "Complete the Command Line Tools installation and rerun this script."

  exit 0
fi


# -------------------------------------------------------------------
# Homebrew
# -------------------------------------------------------------------

if ! command -v brew >/dev/null 2>&1; then
  echo_with_prompt "Homebrew is not installed. Installing it now."

  /bin/bash -c "$(
    curl \
      --fail \
      --silent \
      --show-error \
      --location \
      https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
  )"
fi

if [[ -x /opt/homebrew/bin/brew ]]; then
  brew_executable="/opt/homebrew/bin/brew"
elif [[ -x /usr/local/bin/brew ]]; then
  brew_executable="/usr/local/bin/brew"
elif command -v brew >/dev/null 2>&1; then
  brew_executable="$(command -v brew)"
else
  echo_with_prompt "Homebrew was installed but could not be located."
  exit 1
fi

eval "$("${brew_executable}" shellenv)"

echo_with_prompt "Homebrew is available at $(command -v brew)"


# -------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------

echo_with_prompt "Checking Homebrew dependencies"

if brew bundle check --file="${brewfile}"; then
  echo_with_prompt "All Homebrew dependencies are installed."
else
  echo_with_prompt "Installing missing Homebrew dependencies."

  brew bundle install \
    --file="${brewfile}" \
    --no-upgrade
fi


# -------------------------------------------------------------------
# Fonts
# -------------------------------------------------------------------

font_source="${repository_root}/fonts"
font_target="${HOME}/Library/Fonts"

echo_with_prompt "Installing local fonts"

mkdir -p "${font_target}"

while IFS= read -r -d '' font; do
  destination="${font_target}/$(basename "${font}")"

  if [[ ! -f "${destination}" ]] ||
    ! cmp --silent "${font}" "${destination}"; then
    cp -f -- "${font}" "${destination}"
    echo_with_prompt "Installed $(basename "${font}")"
  fi
done < <(
  find "${font_source}" \
    -type f \
    \( -name '*.ttf' -o -name '*.otf' \) \
    -print0
)


# -------------------------------------------------------------------
# Post-installation configuration
# -------------------------------------------------------------------

git lfs install

echo_with_prompt "Homebrew dependency installation complete."
