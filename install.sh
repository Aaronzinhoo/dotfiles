#!/bin/bash

. "$( pwd )/utils.sh"

echo_with_prompt "Initializing..."

GOPATH="${HOME}/development/go"
PYENV_ROOT="${HOME}/.pyenv"

PATH_TO_ORG="${HOME}/org"
EMACS_INSTALL_DIR="${HOME}/.emacs.d"

# zsh custom plugin folder
CUSTOM_PLUGINS=$HOME/.oh-my-zsh/custom/plugins

if [ -z "$1" ]; then
	INSTALL_ENVIRONMENT="personal"
else
	INSTALL_ENVIRONMENT="work"
fi

echo_with_prompt "Making an necessary folders"

check_and_mkdir "$GOPATH"
check_and_mkdir "$PATH_TO_ORG"
check_and_mkdir "${CUSTOM_PLUGINS}"
check_and_mkdir "${CUSTOM_PLUGINS}/poetry"
check_and_mkdir "$HOME/.local/bin"

echo_with_prompt "Installing..."

source "( pwd )/bootstrap.sh"
