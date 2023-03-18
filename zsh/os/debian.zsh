# KEYCHAIN FOR SSH CONFIG
keychain --quiet $HOME/.ssh/github/$(ls -I "*.*" "$HOME/.ssh/github") > /dev/null
source "$HOME/.keychain/$(hostname)-sh" > /dev/null

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
