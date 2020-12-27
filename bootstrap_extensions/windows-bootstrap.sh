#!/bin/bash
source "$( pwd )/utils.sh"

PROMPT="[ WindowsExtensionLoader ]"

if [ "$1" = "--work" ]; then
	echo_with_prompt "using work windows terminal settings"
	cp $( pwd )/windows/settings_work.json /mnt/c/Users/Aaronzinho/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json
else
	echo_with_prompt "using personal windows terminal settings"
	cp $( pwd )/windows/settings.json /mnt/c/Users/Aaronzinho/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/
fi
