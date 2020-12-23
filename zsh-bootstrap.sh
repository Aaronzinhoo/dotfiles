. "$( pwd )/utils.sh"
PROMPT="[ ZSHBootstrap ]: "
PACKAGE_TO_CHECK="git"

check_system_package_installed $PACKAGE_TO_CHECK
if [ $? -eq 1 ];then
    echo_with_prompt "[FAIL] ${PACKAGE_TO_CHECK} not installed"
    return 1
fi

CUSTOM_PLUGINS=$HOME/.oh-my-zsh/custom/plugins

if [ ! -d "${CUSTOM_PLUGINS}" ]; then
    mkdir -p "${CUSTOM_PLUGINS}"
fi

echo_with_prompt "cloning necessary repos for OMZ and ZSH"

sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

git clone https://github.com/zsh-users/zsh-autosuggestions "${CUSTOM_PLUGINS}"/zsh-autosuggestions

git clone https://github.com/mattberther/zsh-pyenv "${CUSTOM_PLUGINS}"/zsh-pyenv

git clone https://github.com/zsh-users/zsh-completions "${CUSTOM_PLUGINS}"/zsh-completions

git clone https://github.com/zdharma/fast-syntax-highlighting.git
"${CUSTOM_PLUGINS}"/fast-syntax-highlighting

git clone https://github.com/agkozak/zsh-z "${CUSTOM_PLUGINS}"/zsh-z

git clone https://github.com/TamCore/autoupdate-oh-my-zsh-plugins "${CUSTOM_PLUGINS}"/autoupdate
