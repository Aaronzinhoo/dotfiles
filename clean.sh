#!/bin/bash

export XDG_CONFIG_HOME="${HOME}/.config"

export CARGO_HOME="${XDG_CONFIG_HOME}/cargo"
export GOENV_GOPATH_PREFIX="${XDG_CONFIG_HOME}/go"
export GOENV_ROOT="${XDG_CONFIG_HOME}/goenv"
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
export PYENV_ROOT="${XDG_CONFIG_HOME}/pyenv"
export RUSTUP_HOME="${XDG_CONFIG_HOME}/multirust"
export SDKMAN_DIR="${XDG_CONFIG_HOME}/sdkman"

rm -r ~/.npm
rm -r "${CARGO_HOME}"
rm -r "${GOENV_GOPATH_PREFIX}"
rm -r "$(goenv root)"
rm -r "${XDG_CONFIG_HOME}/nvm"
rm -r "$(pyenv root)"
rm -r "${RUSTUP_HOME}"
rm -r "${XDG_CONFIG_HOME}/sdkman"
