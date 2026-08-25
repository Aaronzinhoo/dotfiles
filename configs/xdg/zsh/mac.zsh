# env vars to install confluent kafka on M1
if (( $+commands[brew] )); then
  librdkafka_prefix="$(brew --prefix librdkafka 2>/dev/null)"

  if [[ -n "${librdkafka_prefix}" ]]; then
    export LIBRARY_PATH="${librdkafka_prefix}/lib${LIBRARY_PATH:+:${LIBRARY_PATH}}"
    export C_INCLUDE_PATH="${librdkafka_prefix}/include${C_INCLUDE_PATH:+:${C_INCLUDE_PATH}}"
  fi
  export FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
  export PKG_CONFIG_PATH=$(brew --prefix)/opt/openssl@3/lib/pkgconfig:/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
fi


# chrome
alias chrome="open -a Google\ Chrome"
