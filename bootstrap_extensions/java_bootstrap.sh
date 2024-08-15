. "$( pwd )/utils.sh"

PROMPT="[ JavaExtensionLoader ]: "

install_bootstrap_check "[ -f ${SDKMAN_DIR}/bin/sdkman-init.sh ]" "java" || exit 0;

echo_with_prompt "Installing SDKMAN at $SDKMAN_DIR"

curl -s "https://get.sdkman.io" | bash
source "$SDKMAN_DIR/bin/sdkman-init.sh"

[[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]] && source "${SDKMAN_DIR}/bin/sdkman-init.sh"

echo_with_prompt "Using SDKMAN version $(sdk version)"
echo_with_prompt "Installing Java packages for emacs | cli"

sdk install java "${JAVA_VERSION}"
sdk install maven
sdk install gradle
sdk install springboot

echo_with_prompt "Finished Installing Java Packages"
