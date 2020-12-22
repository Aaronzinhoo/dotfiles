# PYTHON POETRY
export PATH="$HOME/.poetry/bin:$PATH"

# GOLANG PATH
export PATH=$PATH:/usr/local/go/bin:$HOME/development/go/bin

# DBUS connection
eval $(dbus-launch)
export DBUS_SESSION_BUS_ADDRESS
