eval "$(/opt/homebrew/bin/brew shellenv)"

export FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig

source $(brew --prefix nvm)/nvm.sh
