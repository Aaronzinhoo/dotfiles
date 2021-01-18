#!/bin/bash

# get all utility functions
. "$( pwd )/utils.sh"

PROMPT='[ AptInstaller ]: '


# check user is running as root
if [ "$(whoami)" != "root" ]; then
    echo_with_prompt "[FAIL]: must run with root privledges\n";
    return 1;
fi


# mandatory setup for installing other packages
apt update -y
apt upgrade -y

#--------------------
# necessary packages
#--------------------
echo_with_prompt "installing necessary packages\n";
apt install dnsutils -y
apt install keychain -y
apt install git -y
apt install strace -y
apt install net-tools -y
apt install dbus-x11
apt install curl -y
apt install wget -y
apt install build-essential -y
apt-get install autoconf automake -y
apt-get install fd-find -y
apt-get install fzf -y
apt install bat -y
apt-get download ripgrep
dpkg --force-overwrite -i ripgrep*.deb
rm ripgrep*.deb
apt install xclip -y
apt-get install software-properties-common
#--------------------

#--------------------
# shell configuration
#--------------------
echo_with_prompt "installing zsh\n"
apt install zsh -y
#--------------------

#--------------------
# emacs
#--------------------
echo_with_prompt "installing Emacs\n";
add-apt-repository ppa:kelleyk/emacs -y
apt update -y
apt install emacs27 -y
#--------------------


#--------------------
# org & pdf-tools
#--------------------
echo_with_prompt "installing org packages\n"
apt install phantomjs -y
apt install texlive-latex-base -y
apt install texlive-latex-recommended -y
apt install texlive-fonts-extra -y
apt install texlive-latex-extra -y
apt install libpng-dev zlib1g-dev -y
apt install libpoppler-glib-dev -y
apt install libpoppler-private-dev -y
apt install imagemagick -y
#--------------------

#---------------------
# docker
#---------------------
echo_with_prompt "installing docker\n"
apt install apt-transport-https ca-certificates software-properties-common -y
apt install docker-compose -y
#---------------------



#---------------------
# misc
#---------------------
## Show directory structure with excellent formatting
echo_with_prompt "installing misc. packages\n"
apt install tree -y
apt install tmux -y
apt install fonts-hack-ttf -y
## Htop
apt install htop -y
#---------------------



#--------------------
# languages
#--------------------

## python
echo_with_prompt "installing pyenv dependencies";
apt-get install --no-install-recommends make \
        libssl-dev \
        zlib1g-dev \
        libbz2-dev \
        libreadline-dev \
        libsqlite3-dev \
        llvm \
        libncurses5-dev \
        xz-utils \
        tk-dev \
        libxml2-dev \
        libxmlsec1-dev \
        libffi-dev \
        liblzma-dev -y;

## golang
echo_with_prompt "installing golang"
if [ "${DISTRO_VERSION}" != "20.04" ]; then
    echo_with_prompt "adding more UTD version of GoLang"
    add-apt-repository ppa:longsleep/golang-backports
fi
apt install golang -y

## c++
echo_with_prompt "installing c++ dependencies"
apt install ccls -y
