. "$( pwd )/utils.sh"

PROMPT="[ JavaExtensionLoader ]: "

if [ -f "${SDKMAN_DIR}/bin/sdkman-init.sh" ]; then
    echo_with_prompt "Bootstrapping for java seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping java bootstrapping!"
        exit 0;
    fi
fi

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
