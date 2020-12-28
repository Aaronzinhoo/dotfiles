. "$( pwd )/utils.sh"
PROMPT="[ PyenvExtensionLoader ]: "
PACKAGE_TO_CHECK="git"

check_system_package_installed $PACKAGE_TO_CHECK
if [ $? -eq 1 ];then
    echo_with_prompt "[FAIL] ${PACKAGE_TO_CHECK} not installed"
    return 1
fi

echo_with_prompt "cloning necessary repos for pyenv"

### pyenv
PYENV_ROOT=$HOME/.pyenv
git clone https://github.com/pyenv/pyenv.git "${PYENV_ROOT}"
git clone https://github.com/pyenv/pyenv-virtualenv.git "${PYENV_ROOT}"/plugins/pyenv-virtualenv
