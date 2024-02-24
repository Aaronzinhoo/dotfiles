#!/bin/bash

# get all utility functions
. "$( pwd )/utils.sh"

export PROMPT='[ BrewInstaller ]: '

# install brew if needed
echo_with_prompt "Verifying HomeBrew is installed"
if which brew > /dev/null; then
    echo_with_prompt "HomeBrew is installed! Continuing with installation of packages"
else
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# mandatory setup for installing other packages
brew update

#--------------------
# necessary packages
#--------------------
echo_with_prompt "installing necessary packages\n";
brew install bind git coreutils dbus wget autoconf automake fd fzf bat ripgrep pandoc git-lfs enchant pkg-config
#--------------------

#--------------------
# shell configuration
#--------------------
echo_with_prompt "installing shell dependencies\n"
brew install shellcheck
#--------------------

#--------------------
# emacs
#--------------------
echo_with_prompt "installing Emacs\n";
brew tap d12frosted/emacs-plus
if command -v emacs &> /dev/null; then
    brew uninstall emacs-plus
fi
brew install emacs-plus@28 --with-ctags --with-dbus --with-xwidgets --with-imagemagick --with-native-comp --with-no-titlebar --with-mailutils
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
brew install homebrew/cask/docker
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
# kustomize
#----------------------
echo_with_prompt "installing kustomize\n"
brew install kustomize
#----------------------

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
ln -s /opt/homebrew/opt/postgresql@14/lib/postgresql@14/libpq.5.dylib /usr/local/lib/libpq.5.dylib
#----------------------


#---------------------
# qutebrowser
#---------------------
echo_with_prompt "installing qutebrowser"
brew install --cask qutebrowser
#---------------------

#---------------------
# install chrome for web development
#---------------------
brew install --cask google-chrome
brew install --cask chromedriver
#---------------------


#---------------------
# misc
#---------------------
## Show directory structure with excellent formatting
echo_with_prompt "installing misc. packages\n"
brew install tree
brew install tmux
brew tap homebrew/cask-fonts
brew install --cask font-hack-nerd-font
# psycopg2 M1 support
brew install libpq --build-from-source


## Htop
brew install htop
brew install yamllint
#---------------------



#--------------------
# languages
#--------------------

## python
echo_with_prompt "installing pyenv dependencies";
brew install pyenv
brew install pyenv-virtualenv
brew install jawshooah/pyenv/pyenv-default-packages

## golang
echo_with_prompt "installing golang"
brew install go
ENV PATH="${PATH}:/usr/local/go/bin"

## C++
echo_with_prompt "installing C++ dependencies"
brew install ccls
brew install cmake
# gbd is not compatible on mac
#brew install gdb

## Rust
echo_with_prompt "installing Rust dependencies";
brew install rustup

## Java
echo_with_prompt "installing Java dependencies";
brew tap spring-io/tap
brew install maven
brew install gradle

# ------------------
## Dockerfile
# ------------------
echo_with_prompt "Installing Docker dependencies";
brew install hadolint
# ------------------

# ------------------
# networking
# ------------------
brew install mtr
brew install dnsmap
brew install nmap
# ------------------


# ------------------
# kafka
# ------------------
brew install openssl
brew install librdkafka
brew install pkg-config
# ------------------
