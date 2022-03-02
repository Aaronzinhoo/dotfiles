. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

echo_with_prompt "installing python for use in pyenv"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

pyenv install 3.9.10
pyenv global 3.9.10

eval "$(pyenv init --path)"

echo_with_prompt "install grip for markdown preview"
pip install virtualenvwrapper
pip install ipython
pip install psycopg2-binary
pip install pg_activity
