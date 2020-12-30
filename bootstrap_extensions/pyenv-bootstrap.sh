. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

echo_with_prompt "installing python for use in pyenv"

pyenv install 3.8.6
pyenv global 3.8.6
