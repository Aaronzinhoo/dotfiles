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

PROMPT="[ PythonExtensionLoader ]: "

# ensure env vars set
: "${PYENV_ROOT:?PYENV_ROOT must be configured}"
: "${PYTHON_ROOT:?PYTHON_ROOT must be configured}"
: "${PYTHON_VERSION}:?PYTHON_VERSION must be configured"

PACKAGES=(
  "ansible"
  "basedpyright"
  "black"
  "cookiecutter"
  "httpie"
  "ipython"
  "matplotlib"
  "mypy"
  "poetry"
  "pre-commit"
  "ruff"
)

install_pyenv() {
  export PATH="$PYENV_ROOT/bin:$PATH"

  # configure openssl
  openssl_prefix="$(brew --prefix openssl@3)"
  export LDFLAGS="-L${openssl_prefix}/lib"
  export CPPFLAGS="-I${openssl_prefix}/include"
  export PKG_CONFIG_PATH="${openssl_prefix}/lib/pkgconfig"
  export CONFIGURE_OPTS="--with-openssl=${openssl_prefix}"

  echo_with_prompt "Installing pyenv"

  if [[ -x "${PYENV_ROOT}/bin/pyenv" ]]; then
    echo_with_prompt "pyenv already installed"
    return
  else
    curl --fail --location --silent --show-error https://pyenv.run | bash
  fi


  # install pyenv extensions for default package management
  if [[ ! -d "$(pyenv root)/plugins/pyenv-default-packages" ]]; then
    git clone https://github.com/jawshooah/pyenv-default-packages.git "$(pyenv root)/plugins/pyenv-default-packages"
  else
    echo_with_prompt "pyenv-default-packages installed already"
  fi

  # Initialize pyenv for this running Bash process.
  eval "$(pyenv init - bash)"

  if pyenv commands | command grep -qx 'virtualenv-init'; then
    eval "$(pyenv virtualenv-init -)"
  fi
}

configure_python_packages() {
  printf '%s\n' \
    "${PACKAGES[@]}" \
    >"${PYENV_ROOT}/default-packages"
}

install_python() {
  echo_with_prompt "Installing python ${PYTHON_VERSION}"
  pyenv install --skip-existing "${PYTHON_VERSION}"
  pyenv global "${PYTHON_VERSION}"
  pyenv rehash

  export PYENV_VERSION="${PYTHON_VERSION}"

  echo_with_prompt "Installing python dependencies"
}

install_python_dependencies() {
  pip install --upgrade pip
  pip install virtualenvwrapper
  pip install -r "$(pyenv root)/plugins/pyenv-default-packages"
}

main() {
  install_pyenv
  configure_default_packages
  install_python
  install_python_dependencies
  echo_with_green_prompt "Finished python bootstrapping!"
}

main "@"
