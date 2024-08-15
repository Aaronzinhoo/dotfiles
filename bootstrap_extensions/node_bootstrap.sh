. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

install_bootstrap_check "[ -f $NVM_DIR/nvm.sh ]" "node" || exit 0;

echo_with_prompt "Installing nvm at $NVM_DIR"

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash

echo_with_prompt "NVM home dir: $NVM_DIR"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

cat >"$NVM_DIR/default-packages"<< EOF
typescript
jsonlint
@angular/cli
prettier
import-js
eslint
stylelint
stylelint-config-standard
stylelint-scss
@typescript-eslint/eslint-plugin
@typescript-eslint/parser
@angular/language-service@next
@angular/language-server
@asyncapi/cli
bash-language-server
yaml-language-server
tsun
dockerfile-language-server-nodejs
@ansible/ansible-language-server
redoc-cli
pyright
EOF

nvm install "$NODE_VERSION"
nvm alias default node
