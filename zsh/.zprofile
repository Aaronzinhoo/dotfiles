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

# if using wsl
# if [ ! -z "$WSL_DISTRO_NAME" ]; then
# source $ZSH_CUSTOM_SETUP_DIR/os/wsl.zsh
# fi

# OS specific setup
case $OSTYPE in
    darwin*)
        source $ZSH_CUSTOM_SETUP_DIR/os/mac.zsh
	    ;;
    *)
	    echo "OS type $OSTYPE does not have an OS specific configuration"
	    ;;
esac
