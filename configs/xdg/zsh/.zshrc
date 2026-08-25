# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# Powerlevel10k instant prompt must remain near the top.
if [[ -z "${INSIDE_EMACS:-}" ]] &&
   [[ -r "${XDG_CACHE_HOME}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Add generated completion definitions before Oh My Zsh runs compinit.
completion_directory="${ZDOTDIR}/completions"
if [[ -d "${completion_directory}" ]]; then
  fpath=("${completion_directory}" $fpath)
fi

export ZSH_COMPDUMP="${XDG_CACHE_HOME}/zsh/zcompdump-${ZSH_VERSION}"

# Interactive aliases.
[[ -r "${HOME}/.alias" ]] &&
  source "${HOME}/.alias"

# zsh-z configuration.
export ZSHZ_DATA="${XDG_DATA_HOME}/zsh-z/history"
export ZSHZ_CASE=smart

# zsh-nvm configuration. Do not manually source nvm.sh when using zsh-nvm's lazy loading.
export NVM_AUTO_USE=true
export NVM_LAZY_LOAD=true
export NVM_LAZY_LOAD_EXTRA_COMMANDS=(emacs emacsclient)
export NVM_COMPLETION=true

# Other interactive tool settings.
export PIPENV_PYTHON="${PYENV_ROOT}/shims/python"
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV=true
export DOCKER_BUILDKIT=1

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
ZSH_CUSTOM_AUTOUPDATE_QUIET=true

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  autoupdate
  azure
  colored-man-pages
  command-not-found
  docker
  docker-compose
  dotenv
  git
  golang
  helm
  kubectl
  opentofu
  pip
  pyenv
  rust
  spring
  zsh-autosuggestions
  zsh-better-npm-completion
  zsh-completions
  zsh-nvm
  zsh-syntax-highlighting
  zsh-z
)

# Disable some plugins while running in Emacs
if [[ -n "${INSIDE_EMACS:-}" ]]; then
  plugins=(
    azure
    git
    zsh-z
    zsh-nvm
    pyenv
    golang
    docker
    helm
    kubectl
    opentofu
  )
  ZSH_THEME='simple'
else
  ZSH_THEME='powerlevel10k/powerlevel10k'
fi

source "$ZSH/oh-my-zsh.sh"

if (( $+commands[pyenv] )); then
  if pyenv commands |
      command grep -qx 'virtualenv-init'; then
    eval "$(pyenv init - zsh)"
    eval "$(pyenv virtualenv-init -)"
  fi
fi

if (( $+commands[goenv] )); then
  eval "$(goenv init - zsh)"
fi

# custom key binding CTRL+SPC for accepting
if (( ${+widgets[autosuggest-accept]} )); then
  bindkey '^ ' autosuggest-accept
fi

if [[ -z "${INSIDE_EMACS:-}" ]] &&
   [[ -r "${ZDOTDIR}/.p10k.zsh" ]]; then
  source "${ZDOTDIR}/.p10k.zsh"
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# completion setup
if (( $+commands[minikube] )); then
  source <(minikube completion zsh)
fi
if (( $+commands[argocd] )); then
  source <(argocd completion zsh)
fi
if (( $+commands[gh] )); then
  source <(gh completion --shell zsh)
fi


# emacs vterm setup
# Some of the most useful features in emacs-libvterm require shell-side
# configurations. The main goal of these additional functions is to enable the
# shell to send information to `vterm` via properly escaped sequences. A
# function that helps in this task, `vterm_printf`, is defined below.
vterm_printf() {
  if [[ -n "${TMUX:-}" ]] &&
     [[ "${TERM%%-*}" == 'tmux' || "${TERM%%-*}" == 'screen' ]]; then
    printf '\ePtmux;\e\e]%s\007\e\\' "$1"
  elif [[ "${TERM%%-*}" == 'screen' ]]; then
    printf '\eP\e]%s\007\e\\' "$1"
  else
    printf '\e]%s\e\\' "$1"
  fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "${INSIDE_EMACS:-}" == 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback"; tput clear'

  setopt PROMPT_SUBST
  PROMPT="${PROMPT}"'%{$(vterm_prompt_end)%}'
fi

# With vterm_cmd you can execute Emacs commands directly from the shell.
# For example, vterm_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# vterm-eval-cmds.
vterm_cmd() {
  local vterm_elisp
  vterm_elisp=""
  while [ $# -gt 0 ]; do
    vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
    shift
  done
  vterm_printf "51;E$vterm_elisp"
}

# This is to change the title of the buffer based on information provided by the
# shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
# various symbols.
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt
vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

if [[ "${INSIDE_EMACS:-}" == 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback"; tput clear'

  setopt PROMPT_SUBST
  PROMPT="${PROMPT}"'%{$(vterm_prompt_end)%}'
fi

# AsyncAPI CLI Autocomplete
asyncapi_completion="${XDG_CACHE_HOME}/@asyncapi/cli/autocomplete/zsh_setup"

if [[ -r "${asyncapi_completion}" ]]; then
  source "${asyncapi_completion}"
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
[[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]] && source "${SDKMAN_DIR}/bin/sdkman-init.sh"
