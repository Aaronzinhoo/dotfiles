# # KEYCHAIN FOR SSH CONFIG
keychain --quiet $HOME/.ssh/github/$(ls -I "*.*" "$HOME/.ssh/github") > /dev/null
source "$HOME/.keychain/$(hostname)-sh" > /dev/null

# DBUS connection
eval $(dbus-launch)
export DBUS_SESSION_BUS_ADDRESS

# ENV VARS FOR X SERVER
if [ ! -z "$WSL_DISTRO_NAME" ] && [ ! -z "$WSL_IF_IP" ]; then
	export DISPLAY="${WSL_IF_IP}:0.0"
else
	export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0.0
fi

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# golang
export PATH="/usr/local/go/bin:$PATH"
