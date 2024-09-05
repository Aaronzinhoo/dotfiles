. "$( pwd )/utils.sh"

PROMPT="[ ZSHExtensionLoader ]: "

install_bootstrap_check "[ -d $HOME/.oh-my-zsh ]" "zsh" || exit 0;

PACKAGE_TO_CHECK="git"
if ! command -v "$PACKAGE_TO_CHECK" > /dev/null; then
    echo_with_prompt "[FAIL] ${PACKAGE_TO_CHECK} not installed"
    exit 1
fi

CUSTOM_PLUGINS="$HOME/.oh-my-zsh/custom/plugins"

echo_with_prompt "installing OMZ..."
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended --keep-zshrc

echo_with_prompt "cloning plugins and themes for OMZ and ZSH"

git clone --depth=1 https://github.com/romkatv/powerlevel10k.git "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k"

git clone https://github.com/zsh-users/zsh-autosuggestions "${CUSTOM_PLUGINS}/zsh-autosuggestions"

git clone https://github.com/mattberther/zsh-pyenv "${CUSTOM_PLUGINS}/zsh-pyenv"

git clone https://github.com/zsh-users/zsh-completions "${CUSTOM_PLUGINS}/zsh-completions"

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${CUSTOM_PLUGINS}/zsh-syntax-highlighting"

git clone https://github.com/agkozak/zsh-z "${CUSTOM_PLUGINS}/zsh-z"

git clone https://github.com/TamCore/autoupdate-oh-my-zsh-plugins "${CUSTOM_PLUGINS}/autoupdate"

git clone https://github.com/lukechilds/zsh-nvm "${CUSTOM_PLUGINS}/zsh-nvm"

git clone https://github.com/lukechilds/zsh-better-npm-completion "${CUSTOM_PLUGINS}/zsh-better-npm-completion"

git clone https://github.com/johanhaleby/kubetail.git "${CUSTOM_PLUGINS}/kubetail"


echo_with_prompt "Adding necessary symlinks"
if [ ! -L "$HOME/.local/bin/bat" ]; then
    echo_with_prompt "linking batcat"
    ln -s /usr/bin/batcat $HOME/.local/bin/bat
fi

if [ -n "$WSL_DISTRO_NAME" ]; then
    echo_with_prompt "changing shell to ZSH lovliness"
    chsh -s $(which zsh)
fi

echo_with_prompt "Completed OMZ bootstrapping"
