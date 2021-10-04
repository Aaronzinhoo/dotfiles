. "$( pwd )/utils.sh"

PROMPT="[ JavaExtensionLoader ]: "

echo_with_prompt "installing Java packages for emacs | cli"

source "~/.sdkman/bin/sdkman-init.sh"
sdk install springboot
