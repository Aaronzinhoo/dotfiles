eval "$(${HOMEBREW_ROOT}/bin/brew shellenv)"

export FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

export PKG_CONFIG_PATH=$(brew --prefix)/opt/openssl@3/lib/pkgconfig:/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig

# chrome
alias chrome="open -a Google\ Chrome"

# postgres
export LDFLAGS="-L$(brew --prefix)/opt/openssl@1.1/lib"
