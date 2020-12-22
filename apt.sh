#!/bin/bash

# get all utility functions
. "$( pwd )/utils.sh"

PROMPT='[ AptInstaller ]: '


# check user is running as root
if [ "$(whoami)" != "root" ]; then
    echo_with_prompt "run with root privledges\n";
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
apt install git -y
apt install strace -y
apt install net-tools -y
apt install curl -y
apt install wget -y
apt install build-essential -y

#--------------------
# shell configuration
sudo apt install zsh
sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
#--------------------

#--------------------
# emacs
#--------------------
echo_with_prompt "installing Emacs\n";
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update -y
sudo apt install emacs27 -y

#---------------------
# docker
#---------------------
apt install apt-transport-https ca-certificates curl software-properties-common -y
apt install docker-compose -y
#---------------------



#---------------------
# misc
#---------------------
## Show directory structure with excellent formatting
apt install tree -y
apt install tmux -y
apt install fonts-hack-ttf
## Htop
apt install htop -y
#---------------------



#--------------------
# languages
#--------------------

## nodejs
echo_with_prompt "installing nvm\n";
curl https://raw.githubusercontent.com/creationix/nvm/master/install.sh | bash

## python
echo_with_prompt "installing python dependencies";
sudo apt-get install --no-install-recommends make \
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
     liblzma-dev;

### pyenv
echo_with_prompt "installing pyenv"
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.profile
echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.profile
. ~/.profile
git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv

## golang
curl -O https://storage.googleapis.com/golang/go1.15.6.linux-amd64.tar.gz
tar -C /usr/local -xzf go1.15.6.linux-amd64.tar.gz
mv go /usr/local

## c++
apt install ccls
