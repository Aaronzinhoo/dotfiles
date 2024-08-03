. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

if [ -f $NVM_DIR/nvm.sh ]; then
    echo_with_prompt "Bootstrapping for nvm seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp      
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping nvm bootstrapping!"
        exit 0;
    fi
fi

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
EOF

nvm install "$NODE_VERSION"
nvm alias default node
