. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

echo_with_prompt "installing python for use in pyenv"

pyenv install $PYTHON_VERSION
pyenv global $PYTHON_VERSION
