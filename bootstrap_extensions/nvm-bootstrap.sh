. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

echo_with_prompt "bootstrapping nvm setup"

echo_with_prompt "NVM home dir: $NVM_DIR"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

{
    echo -n "typescript -g"
    echo -n "jsonlint -g"
    echo -n "@angular/cli -g"
    echo -n "prettier -g"
    echo -n "import-js -g"
    echo -n "eslint -g"
    echo -n "stylelint -g"
    echo -n "stylelint-config-standard -g"
    echo -n "stylelint-scss -g"
    echo -n "@typescript-eslint/eslint-plugin -g"
    echo -n "@typescript-eslint/parser -g"
    echo -n "@angular/language-service@next -g"
    echo -n "@angular/language-server -g"
    echo -n "@asyncapi/cli -g"
    echo -n "vmd -g"
    echo -n "bash-language-server -g"
    echo -n "yaml-language-server -g"
    echo -n "tsun -g"
    echo -n "dockerfile-language-server-nodejs -g"
    echo -n "@ansible/ansible-language-server -g"
} > "$NVM_DIR/default-packages"

nvm install --lts
nvm alias default lts
