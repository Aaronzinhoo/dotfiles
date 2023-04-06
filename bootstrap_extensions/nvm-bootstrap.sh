. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

echo_with_prompt "installing typescript server for emacs"

echo_with_prompt "NVM home dir: $NVM_DIR"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

nvm install 'lts/*' --reinstall-packages-from=current
nvm alias default lts

npm install -g typescript
npm install -g jsonlint
npm install -g @angular/cli
npm install -g prettier
npm install -g import-js
npm install -g eslint
npm install -g stylelint stylelint-config-standard stylelint-scss
npm install -g @typescript-eslint/eslint-plugin
npm install -g @typescript-eslint/parser
npm install -g @angular/language-service@next
npm install -g @angular/language-server
npm install -g @asyncapi/cli

# bash
npm install -g bash-language-server

# yaml
npm install -g yaml-language-server

# ts-compint/REPL
npm install -g tsun

# docker language server & syntax checker
npm i -g dockerfile-language-server-nodejs
