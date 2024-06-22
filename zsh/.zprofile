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

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# nvm
export NVM_DIR="$XDG_CONFIG_HOME/nvm"

# golang
export PATH="/usr/local/go/bin:$PATH"

# OS speific setup
if [[ "$OSTYPE" =~ "^darwin" ]]; then
    source $ZSH_CUSTOM_SETUP_DIR/os/mac.zsh
fi
