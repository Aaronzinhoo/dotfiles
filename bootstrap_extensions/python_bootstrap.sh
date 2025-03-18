. "$( pwd )/utils.sh"

PROMPT="[ PyenvExtensionLoader ]: "

install_bootstrap_check "[ -f ${PYENV_ROOT}/default-packages ]" "python" || exit 0;

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
ansible
EOF

pyenv install "${PYTHON_VERSION}"
pyenv global "${PYTHON_VERSION}"

pip install --upgrade pip
pip install virtualenvwrapper
