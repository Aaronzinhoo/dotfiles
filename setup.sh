#!/bin/bash

. "$( pwd )/utils.sh"

PROMPT="[ Setup ]: "

echo_with_prompt "Initializing..."

PATH_TO_ORG="${HOME}/development/org"
PATH_TO_DEVELOPMENT="${HOME}/development"

if [ -z "$1" ]; then
    export INSTALL_ENVIRONMENT="personal"
else
    export INSTALL_ENVIRONMENT="work"
fi

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${XDG_CONFIG_HOME}/cache"
export XDG_DATA_HOME="${XDG_CONFIG_HOME}/local/share"
EMACS_INSTALL_DIR="${HOME}/.emacs.d"
export EDITOR="emacs"

export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
export GOENV_ROOT="${XDG_CONFIG_HOME}/goenv"
export GOENV_GOPATH_PREFIX="${XDG_CONFIG_HOME}/go"
export PYENV_ROOT="${XDG_CONFIG_HOME}/pyenv"
export SDKMAN_DIR="${XDG_CONFIG_HOME}/sdkman"
export ZHSZ_DATA="${XDG_CONFIG_HOME}/z"
export PYTHON_VERSION="3.12.4"
export NODE_VERSION="20.15.0"
export GO_VERSION="1.22.4"
export EMACS_VERSION="30"

echo_with_prompt "Making needed folders"

check_and_mkdir "$GOPATH"
check_and_mkdir "$PATH_TO_DEVELOPMENT"
check_and_mkdir "$PATH_TO_ORG"
check_and_mkdir "${PATH_TO_ORG}/notebook"
check_and_mkdir "${PATH_TO_ORG}/references"
check_and_mkdir "${PATH_TO_ORG}/work"
check_and_mkdir "$HOME/.local/bin"
check_and_mkdir "$XDG_CONFIG_HOME"
check_and_mkdir "$XDG_CACHE_HOME"
check_and_mkdir "$XDG_DATA_HOME"
check_and_mkdir "$HOME/.ssh/github"
check_and_mkdir "${XDG_CONFIG_HOME}/emacs/backups"

echo_with_prompt "Installing Packages and Bootstrapping..."

source "$( pwd )/bootstrap.sh"

echo_with_prompt "Finished! Enjoy your new setup"
