. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

echo_with_prompt "installing typescript server for emacs"

echo_with_prompt "NVM home dir: $NVM_DIR"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

nvm install 'lts/*' --reinstall-packages-from=current
nvm use --lts
npm install -g typescript
npm install -g typescript-eslint
npm install -g eslint-plugin-angular
npm install -g eslint-config-angular
npm install -g jsonlint
npm install -g @angular/cli
npm install -g prettier
npm install -g import-js
npm install -g eslint
