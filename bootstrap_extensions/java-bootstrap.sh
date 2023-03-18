. "$( pwd )/utils.sh"

PROMPT="[ JavaExtensionLoader ]: "

echo_with_prompt "installing Java packages for emacs | cli"

case "$OSTYPE" in
    linux-gnu*)
        curl -s "https://get.sdkman.io" | bash
        source "$HOME/.sdkman/bin/sdkman-init.sh"
	    ;;
    darwin*)
        brew tap sdkman/tap
        brew install sdkman-cli
        brew install jenv
	    brew install spring-boot
	    ;;
    *)
	    echo_with_prompt "No compatible OS found, Skipping installing java dependencies"
	    ;;
esac

sdk install java 17.0.6-tem
sdk install maven
sdk install springboot
