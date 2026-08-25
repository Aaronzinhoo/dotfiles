# XDG base directories.
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-${HOME}/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-${HOME}/.local/state}"

# Zsh configuration.
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
export ZSH_CUSTOM_SETUP_DIR="${ZDOTDIR}"

# Homebrew.
export HOMEBREW_ROOT="${XDG_CONFIG_HOME}/homebrew"

# Language and environment managers.
export CARGO_HOME="${XDG_CONFIG_HOME}/cargo"
export RUSTUP_HOME="${XDG_CONFIG_HOME}/rustup"
export GOENV_ROOT="${XDG_CONFIG_HOME}/goenv"
export GOPATH="${XDG_DATA_HOME}/go"
export GOBIN="${GOPATH}/bin"
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
export PYENV_ROOT="${XDG_CONFIG_HOME}/pyenv"
export SDKMAN_DIR="${XDG_CONFIG_HOME}/sdkman"

# Other tools.
export KREW_ROOT="${XDG_DATA_HOME}/krew"

# Personal application data.
export ORG_ROOT="${HOME}/development/org"
export EMACS_DIR="${XDG_CONFIG_HOME}/emacs"
export EMACS_INSTALL_DIR="${HOME}/.emacs.d"
