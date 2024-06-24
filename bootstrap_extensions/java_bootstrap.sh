. "$( pwd )/utils.sh"

PROMPT="[ JavaExtensionLoader ]: "

[[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]] && source "${SDKMAN_DIR}/bin/sdkman-init.sh"

echo_with_prompt "Using SDKMAN version $(sdk version)"
echo_with_prompt "Installing Java packages for emacs | cli"

sdk install java 17.0.6-tem
sdk install maven
sdk install springboot

echo_with_prompt "Finished Installing Java Packages"
