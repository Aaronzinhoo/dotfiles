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
apt-get install plantuml -y
#--------------------

#---------------------
# docker
#---------------------
echo_with_prompt "installing docker\n"
apt install apt-transport-https ca-certificates software-properties-common -y
apt install docker-compose -y
#---------------------

#---------------------
# kubectl
#---------------------
echo_with_prompt "installing kubectl\n"
curl -fsSLo /usr/share/keyrings/kubernetes-archive-keyring.gpg https://packages.cloud.google.com/apt/doc/apt-key.gpg
echo "deb [signed-by=/usr/share/keyrings/kubernetes-archive-keyring.gpg] https://apt.kubernetes.io/ kubernetes-xenial main" | sudo tee /etc/apt/sources.list.d/kubernetes.list
apt-get update
apt-get install kubectl -y
#---------------------

#---------------------
# helm
#---------------------
echo_with_prompt "installing helm\n"
curl https://baltocdn.com/helm/signing.asc | apt-key add -
apt-get install apt-transport-https -y
echo "deb https://baltocdn.com/helm/stable/debian/ all main" | tee /etc/apt/sources.list.d/helm-stable-debian.list
apt-get update -y
apt-get install helm -y
#---------------------

#---------------------
# kustomize
#----------------------
echo_with_prompt "installing kustomize\n"
curl -s "https://raw.githubusercontent.com/kubernetes-sigs/kustomize/master/hack/install_kustomize.sh"  | bash
mv kustomize /usr/local/bin/kustomize
#----------------------

#---------------------
# aws-cli & iam-authenticator
#----------------------
echo_with_prompt "installing aws packages\n"
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
./aws/install
rm -r awscliv2.zip aws
curl -o aws-iam-authenticator https://amazon-eks.s3.us-west-2.amazonaws.com/1.21.2/2021-07-05/bin/linux/amd64/aws-iam-authenticator
chmod +x ./aws-iam-authenticator
mv ./aws-iam-authenticator /usr/local/bin
#----------------------


#---------------------
# qutebrowser
#---------------------
echo_with_prompt "installing qutebrowser"
apt install libtool -y;
apt install libtool-bin -y
apt install qutebrowser -y
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
# batcat (at the time batcat is 17.01 1/18/2021)
export VER="0.17.1"
wget "https://github.com/sharkdp/bat/releases/download/v${VER}/bat_${VER}_amd64.deb"
dpkg -i bat_${VER}_amd64.deb
rm bat_${VER}_amd64.deb
# install chrome for web development
wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
apt update
apt install google-chrome-stable -y
apt install yamllint -y
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

## C++
echo_with_prompt "installing C++ dependencies"
apt install ccls -y
apt install cmake -y
apt install gdb -y

## Rust
echo_with_prompt "installing Rust dependencies";
curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh

## Java
echo_with_prompt "installing Java dependencies";
apt install openjdk-11-jdk-headless
curl -s "https://get.sdkman.io" | bash

## networking
apt install mtr -y
