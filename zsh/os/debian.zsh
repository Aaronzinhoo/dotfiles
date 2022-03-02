# KEYCHAIN FOR SSH CONFIG
keychain --quiet $HOME/.ssh/github/$(ls -I "*.*" "$HOME/.ssh/github") > /dev/null
source "$HOME/.keychain/$(hostname)-sh" > /dev/null
