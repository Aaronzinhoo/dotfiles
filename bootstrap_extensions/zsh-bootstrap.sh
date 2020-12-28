. "$( pwd )/utils.sh"
PROMPT="[ ZSHExtensionLoader ]: "
PACKAGE_TO_CHECK="git"

check_system_package_installed $PACKAGE_TO_CHECK
if [ $? -eq 1 ];then
    echo_with_prompt "[FAIL] ${PACKAGE_TO_CHECK} not installed"
    return 1
fi

echo_with_prompt "installing OMZ..."
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended --keep-zshrc

CUSTOM_PLUGINS=$HOME/.oh-my-zsh/custom/plugins

if [ ! -d "${CUSTOM_PLUGINS}" ]; then
    mkdir -p "${CUSTOM_PLUGINS}"
fi

echo_with_prompt "cloning plugins and themes for OMZ and ZSH"

git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

git clone https://github.com/zsh-users/zsh-autosuggestions "${CUSTOM_PLUGINS}"/zsh-autosuggestions

git clone https://github.com/mattberther/zsh-pyenv "${CUSTOM_PLUGINS}"/zsh-pyenv

git clone https://github.com/zsh-users/zsh-completions "${CUSTOM_PLUGINS}"/zsh-completions

git clone https://github.com/zdharma/fast-syntax-highlighting.git "${CUSTOM_PLUGINS}"/fast-syntax-highlighting

git clone https://github.com/agkozak/zsh-z "${CUSTOM_PLUGINS}"/zsh-z

git clone https://github.com/TamCore/autoupdate-oh-my-zsh-plugins "${CUSTOM_PLUGINS}"/autoupdate

echo_with_prompt "adding poetry completion to zsh"

curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3
if [ ! -d "${CUSTOM_PLUGINS}/poetry" ];then
	mkdir -p "${CUSTOM_PLUGINS}/poetry"
fi
poetry completions zsh > "${CUSTOM_PLUGINS}/poetry/_poetry"

echo_with_prompt "adding necessary symlinks"

if [ ! -d "${HOME}/.local/bin" ]; then
	echo_with_prompt "creating local bin directory"
	mkdir -p $HOME/.local/bin
fi

# link batcat to bat on local
ln -s /usr/bin/batcat $HOME/.local/bin/bat

echo_with_prompt "changing shell to ZSH lovliness"

chsh -s $(which zsh)
