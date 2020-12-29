. "$( pwd )/utils.sh"

PROMPT="[ NodeExtensionLoader ]: "

echo_with_prompt "installing typescript server for emacs"

nvm install 'lts/*' --reinstall-packages-from=current
nvm use --lts
npm install --save typescript prettify
