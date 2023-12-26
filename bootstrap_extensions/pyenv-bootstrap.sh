. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

echo_with_prompt "bootstraping pyenv setup"

export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

{
    echo -n "ipython"
    echo -n "ruff-lsp"
    echo -n "matplotlib"
} > "$(pyenv root)/default-packages"

pyenv install 3.9.10
pyenv global 3.9.10

eval "$(pyenv init --path)"

pip install virtualenvwrapper
