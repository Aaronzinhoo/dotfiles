. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

echo_with_prompt "bootstraping pyenv setup"

export PATH="$PYENV_ROOT/bin:$PATH"

# env vars to get functionality on unix
export PATH="/usr/local/opt/openssl@3/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@3/include"

eval "$(pyenv init --path)"

cat >"$(pyenv root)/default-packages"<<EOF
ipython
ruff-lsp
matplotlib
autopep8
isort
pyflakes
EOF

pyenv install "${PYTHON_VERSION}"
pyenv global "${PYTHON_VERSION}"

pip install --upgrade pip
pip install virtualenvwrapper
