. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

echo_with_prompt "installing typescript server for emacs"

echo_with_prompt "NVM home dir: $NVM_DIR"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

touch "$NVM_DIR/default-packages"
echo "typescript -g" >> "$NVM_DIR/default-packages"
echo "jsonlint -g" >> "$NVM_DIR/default-packages"
echo "@angular/cli -g" >> "$NVM_DIR/default-packages"
echo "prettier -g" >> "$NVM_DIR/default-packages"
echo "import-js -g" >> "$NVM_DIR/default-packages"
echo "eslint -g" >> "$NVM_DIR/default-packages"
echo "stylelint -g" >> "$NVM_DIR/default-packages"
echo "stylelint-config-standard -g" >> "$NVM_DIR/default-packages"
echo "stylelint-scss -g" >> "$NVM_DIR/default-packages"
echo "@typescript-eslint/eslint-plugin -g" >> "$NVM_DIR/default-packages"
echo "@typescript-eslint/parser -g" >> "$NVM_DIR/default-packages"
echo "@angular/language-service@next -g" >> "$NVM_DIR/default-packages"
echo "@angular/language-server -g" >> "$NVM_DIR/default-packages"
echo "@asyncapi/cli -g" >> "$NVM_DIR/default-packages"
echo "vmd -g" >> "$NVM_DIR/default-packages"

# bash
echo "bash-language-server -g" >> "$NVM_DIR/default-packages"

# yaml
echo "yaml-language-server -g" >> "$NVM_DIR/default-packages"

# ts-compint/REPL
echo "tsun -g" >> "$NVM_DIR/default-packages"

# docker language server & syntax checker
echo "dockerfile-language-server-nodejs -g" >> "$NVM_DIR/default-packages"

nvm install --lts
nvm alias default lts
