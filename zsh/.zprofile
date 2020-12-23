# # KEYCHAIN FOR SSH CONFIG
/usr/bin/keychain --quiet $HOME/.ssh/github/id_rsa > /dev/null
source $HOME/.keychain/$HOSTNAME-sh > /dev/null

# DBUS connection
eval $(dbus-launch)
export DBUS_SESSION_BUS_ADDRESS
