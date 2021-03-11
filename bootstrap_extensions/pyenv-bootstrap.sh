. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

echo_with_prompt "installing python for use in pyenv"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

pyenv install 3.8.6
pyenv global 3.8.6

echo_with_prompt "install grip for markdown preview"
pip install grip
pip install virtualenvwrapper
pip install ipython
