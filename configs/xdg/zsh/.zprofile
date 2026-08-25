# Mainly used for setting up paths and login / interactive sessions

# Locale.
export LANG='en_US.UTF-8'

# Editor used by noninteractive tools.
# Preferred editor for local and remote sessions
export EDITOR='emacsclient -t --alternate-editor=emacs'
export VISUAL="${EDITOR}"

prepend_to_path() {
  if [ -d "$1" ]; then
    case ":${PATH}:" in
      *":$1:"*)
        ;;
      *)
        PATH="$1:${PATH}"
        ;;
    esac
  fi
}

# User executables should take precedence over system executables.
prepend_to_path "${HOME}/.local/bin"
prepend_to_path "${PYENV_ROOT}/bin"
prepend_to_path "${GOENV_ROOT}/bin"
prepend_to_path "${GOENV_ROOT}/shims"
prepend_to_path "${GOBIN}"
prepend_to_path "${KREW_ROOT}/bin"
export PATH

# Initialize Homebrew only on macOS.
if [[ "${OSTYPE}" == darwin* ]]; then
  if [[ -x "${HOMEBREW_ROOT}/bin/brew" ]]; then
    eval "$("${HOMEBREW_ROOT}/bin/brew" shellenv)"
  elif [[ -x /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  elif [[ -x /usr/local/bin/brew ]]; then
    eval "$(/usr/local/bin/brew shellenv)"
  fi
fi

# Make Pyenv shims available to login shells.
if command -v pyenv >/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi

# Add Cargo-installed programs to PATH.
if [[ -r "${CARGO_HOME}/env" ]]; then
  . "${CARGO_HOME}/env"
fi

# OS speific setup
if [[ "$OSTYPE" =~ "^darwin" ]]; then
  # ensure file exists and is readable
  [[ -r "${ZDOTDIR}/os/mac.zsh" ]] && source "${ZDOTDIR}/os/mac.zsh"
fi
