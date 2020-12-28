# # KEYCHAIN FOR SSH CONFIG
/usr/bin/keychain --quiet $HOME/.ssh/github/id_ed25519 > /dev/null
source "$HOME/.keychain/$(hostname)-sh" > /dev/null

# DBUS connection
eval $(dbus-launch)
export DBUS_SESSION_BUS_ADDRESS
