eval "$(/opt/homebrew/bin/brew shellenv)"

export FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

export PKG_CONFIG_PATH=/opt/homebrew/opt/openssl@3/lib/pkgconfig:/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig

# postgres
export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"

# java 17 support
export JAVA_HOME=$(/usr/libexec/java_home)

alias chrome="open -a Google\ Chrome"
