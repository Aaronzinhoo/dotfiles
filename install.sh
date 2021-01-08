#!/bin/bash

. "$( pwd )/utils.sh"

echo_with_prompt "Initializing..."

export DISTRO_VERSION=$( echo $WSL_DISTRO_NAME | sed -s 's/Ubuntu-//')
export PYENV_ROOT="${HOME}/.pyenv"
PATH_TO_ORG="${HOME}/org"
PATH_TO_DEVELOPMENT="${HOME}/development"

# zsh custom plugin folder
CUSTOM_PLUGINS=$HOME/.oh-my-zsh/custom/plugins

if [ -z "$1" ]; then
	export INSTALL_ENVIRONMENT="personal"
else
	export INSTALL_ENVIRONMENT="work"
fi

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${XDG_CONFIG_HOME}/cache"
export XDG_DATA_HOME="${XDG_CONFIG_HOME}/local/share"
EMACS_INSTALL_DIR="${HOME}/.emacs.d"
EDITOR="emacs"

export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
export GOPATH="${XDG_CONFIG_HOME}/go"
export ZHSZ_DATA="${XDG_CONFIG_HOME}/z"

PYTHON_VERSION="3.8.6"

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

echo_with_prompt "Installing..."

source "$( pwd )/bootstrap.sh"

echo_with_prompt "Finished! Enjoy your new setup"
