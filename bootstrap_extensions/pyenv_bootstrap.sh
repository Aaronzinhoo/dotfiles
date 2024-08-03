. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

if [ -f "${PYENV_ROOT}/default-packages" ]; then
    echo_with_prompt "Bootstrapping for python seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp      
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping python bootstrapping!"
        exit 0;
    fi
fi

echo_with_prompt "Installing pyenv dependencies"

brew install python-tk

echo_with_prompt "Installing pyenv"

curl https://pyenv.run | bash
git clone https://github.com/jawshooah/pyenv-default-packages.git $(pyenv root)/plugins/pyenv-default-packages

# env vars to get functionality on unix
export PATH="$PYENV_ROOT/bin:$PATH"
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
