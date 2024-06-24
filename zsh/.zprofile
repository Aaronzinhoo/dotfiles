. "$HOME/.cargo/env"
. "$HOME/.alias"
export EDITOR="emacs"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${XDG_CONFIG_HOME}/cache"
export XDG_DATA_HOME="${XDG_CONFIG_HOME}/local/share"

FILE_NAME=".zprofile"
case $OSTYPE in
    darwin*)
        FILE_PATH="$HOME/$FILE_NAME"
	    export ZSH_CUSTOM_SETUP_DIR=$(dirname $(readlink -f $FILE_PATH || echo $FILE_PATH))
	    ;;
    linux-gnu*)
        if [ $0 = '-zsh' ]; then
            export ZSH_CUSTOM_SETUP_DIR="$HOME/dotfiles/zsh"
        else
            export ZSH_CUSTOM_SETUP_DIR=$(dirname $([ -L $0 ] && readlink -f $0 || echo $0))
        fi
	    ;;
esac

# nvm setup
# must be set before zsh-nvm is loaded
export NVM_LAZY_LOAD=true
export NVM_LAZY_LOAD_EXTRA_COMMANDS=('emacs')
export NVM_COMPLETION=true
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

# pyenv vars
export PYENV_ROOT="$HOME/.config/pyenv"
export PIPENV_PYTHON="$PYENV_ROOT/shims/python"
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="TRUE"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv virtualenv-init -)"

# go env vars
export GOENV_ROOT="${XDG_CONFIG_HOME}/goenv"
export GOENV_GOPATH_PREFIX="${XDG_CONFIG_HOME}/go"

# emacs vars
export ORG_ROOT="${HOME}/org"
export EMACS_DIR="${XDG_CONFIG_HOME}/emacs"

# docker is not running properly without this
export DOCKER_BUILDKIT=0

# OS speific setup
if [[ "$OSTYPE" =~ "^darwin" ]]; then
    source $ZSH_CUSTOM_SETUP_DIR/os/mac.zsh
else
    source $ZSH_CUSTOM_SETUP_DIR/os/wsl.zsh
fi

# set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/.local/bin" ] ; then
    PATH="${HOME}/.local/bin:$PATH"
fi

[[ $OSTYPE =~ darwin* ]] && PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
