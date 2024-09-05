#!/bin/bash

. "$( pwd )/utils.sh"

PROMPT="[ Setup ]: "
SHORT_OPTS="e:m::g::j::n::p::"
JAVA_DEFAULT_VERSION="17.0.6-tem"
PYTHON_DEFAULT_VERSION="3.12.4"
NODE_DEFAULT_VERSION="20.15.0"
GO_DEFAULT_VERSION="1.22.4"
EMACS_DEFAULT_VERSION="31"

check_and_mkdir(){
    if [ ! -d "$1" ]; then
        mkdir -p "$1"
    fi
}

help(){
    echo "Usage: ./setup.sh -e [work|personal];"
    echo "Install workstation from scratch on local machine. All versions are coupled to the versions provided by each environment manager."
    echo "I.e. For Java, the --java-version should match an available SDKMAN Java version."
    echo ""
    echo -e "\t-h \t\tDisplay this message"
    echo -e "\t-e \t\tType of environment to setup. Options: [work, personal]"
    echo -e "\t-m \t\tEmacs Version to install; Default: ${EMACS_DEFAULT_VERSION}"
    echo -e "\t-g \t\tGo version to install via goenv; Default: ${GO_DEFAULT_VERSION}"
    echo -e "\t-j \t\tJava version to install via SDKMAN; Default: ${JAVA_DEFAULT_VERSION}"
    echo -e "\t-n \t\tNode version to install via nvm; Default: ${NODE_DEFAULT_VERSION}"
    echo -e "\t-p \t\tPython version to install via pyenv; Default: ${PYTHON_DEFAULT_VERSION}"
    exit 2
}

while getopts $SHORT_OPTS flag; do
    case "${flag}" in
        e)
            if [ "$OPTARG" != "personal" ] && [ "$OPTARG" != "work" ];then
                echo "Invalid option for -e | --env; Valid inputs are: [work, personal]"
                exit 1;
            fi
            export INSTALL_ENVIRONMENT="$OPTARG"
            ;;
        m)
            export EMACS_VERSION="$OPTARG"
            ;;
        g)
            export GO_VERSION="$OPTARG"
            ;;
        j)
            export JAVA_VERSION="$OPTARG"
            ;;
        n)
            export NODE_VERSION="$OPTARG"
            ;;
        p)
            export PYTHON_VERSION="$OPTARG"
            ;;
        ?)
            help;
            ;;
    esac
done

echo_with_prompt "Initializing..."

if [ -z "$EMACS_VERSION" ]; then
    export EMACS_VERSION=${EMACS_DEFAULT_VERSION}
fi
if [ -z "$GO_VERSION" ]; then
    export GO_VERSION=${GO_DEFAULT_VERSION}
fi
if [ -z "$JAVA_VERSION" ]; then
    export JAVA_VERSION=${JAVA_DEFAULT_VERSION}
fi
if [ -z "$NODE_VERSION" ]; then
    export NODE_VERSION=${NODE_DEFAULT_VERSION}
fi
if [ -z "$PYTHON_VERSION" ]; then
    export PYTHON_VERSION=${PYTHON_DEFAULT_VERSION}
fi

echo_with_prompt "INSTALL_ENVIRONMENT=$INSTALL_ENVIRONMENT"
echo_with_prompt "JAVA_VERSION=$JAVA_VERSION"
echo_with_prompt "PYTHON_VERSION=$PYTHON_VERSION"
echo_with_prompt "NODE_VERSION=$NODE_VERSION"
echo_with_prompt "GO_VERSION=$GO_VERSION"
echo_with_prompt "EMACS_VERSION=$EMACS_VERSION"

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${XDG_CONFIG_HOME}/cache"
export XDG_DATA_HOME="${XDG_CONFIG_HOME}/local/share"

EMACS_INSTALL_DIR="${HOME}/.emacs.d"
DEVELOPMENT_DIR_PATH="${HOME}/development"
ORG_DIR_PATH="${HOME}/development/org"
export EDITOR="emacs"
export CARGO_HOME="${XDG_CONFIG_HOME}/cargo"
export HOMEBREW_ROOT="$HOME/.homebrew"
export KREW_ROOT="${XDG_CONFIG_HOME}/krew"
export GOENV_GOPATH_PREFIX="${XDG_CONFIG_HOME}/go"
export GOENV_ROOT="${XDG_CONFIG_HOME}/goenv"
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
export PYENV_ROOT="${XDG_CONFIG_HOME}/pyenv"
export RUSTUP_HOME="${XDG_CONFIG_HOME}/multirust"
export SDKMAN_DIR="${XDG_CONFIG_HOME}/sdkman"
export ZHSZ_DATA="${XDG_CONFIG_HOME}/z"

echo_with_prompt "Making needed folders"

check_and_mkdir "$GOPATH"
check_and_mkdir "$DEVELOPMENT_DIR_PATH"
check_and_mkdir "$ORG_DIR_PATH"
check_and_mkdir "${ORG_DIR_PATH}/notebook"
check_and_mkdir "${ORG_DIR_PATH}/references"
check_and_mkdir "${ORG_DIR_PATH}/work"
check_and_mkdir "$HOME/.local/bin"
check_and_mkdir "$XDG_CONFIG_HOME"
check_and_mkdir "$XDG_CACHE_HOME"
check_and_mkdir "$XDG_DATA_HOME"
check_and_mkdir "$HOME/.ssh/github"
check_and_mkdir "${XDG_CONFIG_HOME}/emacs/backups"
check_and_mkdir "$NVM_DIR"

echo_with_prompt "Installing Packages and Bootstrapping..."

source "$( pwd )/bootstrap.sh"

echo_with_prompt "Finished! Enjoy your new setup"
