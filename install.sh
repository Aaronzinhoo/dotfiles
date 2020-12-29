#!/bin/bash

. "$( pwd )/utils.sh"

echo_with_prompt "Initializing..."

GOPATH="${HOME}/development/go"
PYENV_ROOT="${HOME}/.pyenv"

PATH_TO_ORG="${HOME}/org"

# zsh custom plugin folder
CUSTOM_PLUGINS=$HOME/.oh-my-zsh/custom/plugins

if [ -z "$1" ]; then
	INSTALL_ENVIRONMENT="personal"
else
	INSTALL_ENVIRONMENT="work"
fi

XDG_CONFIG_HOME="${HOME}/.config"
XDG_CACHE_HOME="${XDG_CONFIG_HOME}/.config/cache"
XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
EMACS_INSTALL_DIR="${XDG_CONFIG_HOME}/emacs
"
EDITOR="emacs"

ZDOTDIR="$XDG_CONFIG_HOME/zsh"
HISTFILE="$ZDOTDIR/.zhistory"    # History filepath

PYTHON_VERSION="3.8.6"

echo_with_prompt "Making needed folders"

check_and_mkdir "$GOPATH"
check_and_mkdir "$PATH_TO_ORG"
check_and_mkdir "${CUSTOM_PLUGINS}"
check_and_mkdir "${CUSTOM_PLUGINS}/poetry"
check_and_mkdir "$HOME/.local/bin"
check_and_mkdir "$XDG_CONFIG_HOME"
check_and_mkdir "$XDG_CACHE_HOME"
check_and_mkdir "$XDG_DATA_DIR"
check_and_mkdir "$ZDOTDIR"

echo_with_prompt "Installing..."

source "( pwd )/bootstrap.sh"
