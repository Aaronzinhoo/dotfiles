# ENV VARS FOR X SERVER
if [ ! -z "$WSL_DISTRO_NAME" ] && [ ! -z "$WSL_IF_IP" ]; then
    export DISPLAY="${WSL_IF_IP}:0.0"
else
    export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0.0
fi
