FILE_NAME=".zprofile"
case $OSTYPE in
    darwin*)
        FILE_PATH="$HOME/$FILE_NAME"
	    export ZSH_CUSTOM_SETUP_DIR=$(dirname $(readlink -f $FILE_PATH || echo $FILE_PATH))
	    ;;
    linux-gnu*)
        export ZSH_CUSTOM_SETUP_DIR=$(dirname $([ -L $0 ] && readlink -f $0 || echo $0))
	    ;;
esac

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

# nvm
export NVM_DIR="$HOME/.nvm"

# golang
export PATH="/usr/local/go/bin:$PATH"

# if using wsl
if [ ! -z "$WSL_DISTRO_NAME" ]; then
    source $ZSH_CUSTOM_SETUP_DIR/os/wsl.zsh
fi

# OS specific setup
case $OSTYPE in
    darwin*)
        source $ZSH_CUSTOM_SETUP_DIR/os/mac.zsh
	    ;;
    linux-gnu*)
	    source $ZSH_CUSTOM_SETUP_DIR/os/debian.zsh
	    ;;
    *)
	    echo "unknown OS type"
	    ;;
esac
