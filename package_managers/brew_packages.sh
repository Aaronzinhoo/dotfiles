#!/bin/bash

# get all utility functions
. "$( pwd )/utils.sh"

export PROMPT='[ BrewInstaller ]: '

# install brew if needed

echo_with_prompt "Verifying HomeBrew is installed in ${HOMEBREW_ROOT}"
if [ -f "${HOMEBREW_ROOT}/bin/brew" ]; then
    echo_with_prompt "HomeBrew is installed! Continuing with installation of packages"
    eval "$(${HOMEBREW_ROOT}/bin/brew shellenv)"
else
    echo_with_prompt "HomeBrew NOT installed! Installing now!"
    mkdir "${HOMEBREW_ROOT}" && curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip-components 1 -C ${HOMEBREW_ROOT}
    eval "$(${HOMEBREW_ROOT}/bin/brew shellenv)"
    brew update --force --quiet
    chmod -R go-w "$(brew --prefix)/share/zsh"
fi

#--------------------
# necessary packages
#--------------------
echo_with_prompt "installing necessary packages\n";
brew install svn
svn list  https://svn.code.sf.net/p/netpbm/code/userguide
brew install bind git coreutils dbus wget autoconf sevenzip automake fd fzf bat ripgrep pandoc git-lfs enchant pkg-config theseal/ssh-askpass/ssh-askpass yq
#--------------------

#--------------------
# shell configuration
#--------------------
echo_with_prompt "installing shell dependencies\n"
brew install shellcheck
#--------------------


#--------------------
# org & pdf-tools
#--------------------
echo_with_prompt "installing org packages\n"
brew install phantomjs
brew install poppler
brew install texlive
brew install imagemagick
brew install plantuml
#--------------------


#---------------------
# gh (github cli client)
#---------------------
brew install gh
#---------------------


#---------------------
# docker
#---------------------
echo_with_prompt "installing docker\n"
brew install ca-certificates
# install GUI version of docker desktop
brew install --cask docker
brew install docker-compose
brew install lazydocker
#---------------------

#---------------------
# minikube
#---------------------
echo_with_prompt "installing minikube\n"
brew install minikube
#---------------------


#---------------------
# kubectl
#---------------------
echo_with_prompt "installing kubectl\n"
brew install kubernetes-cli
brew install kubeseal
brew install kubetail
brew install kubespy
brew install fzf # needed for tools
#---------------------


#---------------------
# helm
#---------------------
echo_with_prompt "installing helm\n"
brew install helm
#---------------------

#---------------------
# aws-cli & iam-authenticator
#----------------------
echo_with_prompt "installing aws packages\n"
brew install awscli
brew install aws-iam-authenticator
#----------------------

#----------------------
# postgres
#----------------------
brew install postgresql
brew install pgformatter
if [ ! -L "/usr/local/lib/libpq.5.dylib" ]; then
    ln -s /opt/homebrew/opt/postgresql@14/lib/postgresql@14/libpq.5.dylib /usr/local/lib/libpq.5.dylib
fi
#----------------------


#---------------------
# qutebrowser
#---------------------
echo_with_prompt "installing qutebrowser"
brew install --cask qutebrowser
#---------------------

#---------------------
# chrome
#---------------------
brew install --cask google-chrome
brew install --cask chromedriver
#---------------------


#---------------------
# misc
#---------------------
echo_with_prompt "installing misc. packages\n"
# show directory structure with formatting
brew install tree
brew install tmux
# psycopg2 M1 support
brew install libpq --build-from-source
# fonts
find -E $( pwd ) -regex ".*\.ttf" | xargs -I % -n 2 cp % ~/Library/Fonts/
brew install htop
brew install yamllint
#---------------------


#--------------------
# languages
#--------------------

## C++
echo_with_prompt "installing C++ dependencies"
brew install ccls
brew install cmake

# ------------------
# Dockerfile
# ------------------
echo_with_prompt "Installing Docker dependencies";
brew install hadolint
# ------------------

# ------------------
# networking
# ------------------
echo_with_prompt "installing Networking dependencies";
brew install mtr
brew install dnsmap
brew install nmap
# ------------------


# ------------------
# kafka
# ------------------
echo_with_prompt "installing Kafka dependencies";
brew install openssl
brew install librdkafka
brew install pkg-config
# ------------------
