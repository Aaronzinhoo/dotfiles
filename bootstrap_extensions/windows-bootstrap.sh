#!/bin/bash
source "$( pwd )/utils.sh"

PROMPT="[ WindowsExtensionLoader ]:"

if [ -z "$WSL_DISTRO_NAME" ]; then
	echo_with_prompt "Not using WSL, exiting..."
	return 1
fi

WINDOWS_USERNAME=$(cmd.exe /c echo %username% | tr -d '\r')
WINDOWS_HOME="/mnt/c/Users/${WINDOWS_USERNAME}"

if [ "$INSTALL_ENVIRONMENT" = "work" ]; then
	echo_with_prompt "using work windows terminal settings"
	cp "$( pwd )/windows/settings_work.json" "${WINDOWS_HOME}/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"
else
	echo_with_prompt "using personal windows terminal settings"
	cp "$( pwd )/windows/settings.json" "${WINDOWS_HOME}/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/"
fi

echo_with_prompt "adding vcxsrv script to windows home"
cp "$( pwd )/windows/MyXsrv.bat" "${WINDOWS_HOME}"

echo_with_prompt "adding script to registry"
reg.exe add "HKCU\Software\Microsoft\Windows\CurrentVersion\Run" /v RunXsrv /d $(wslpath -w $(echo "${WINDOWS_HOME}/MyXsrv.bat"))
