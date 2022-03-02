. "$( pwd )/utils.sh"

PROMPT="[ JavaExtensionLoader ]: "

echo_with_prompt "installing Java packages for emacs | cli"

case "$OSTYPE" in
    linux-gnu*)
	    source "$HOME/.sdkman/bin/sdkman-init.sh"
	    sdk install springboot
	    ;;
    darwin*)
	    brew install spring-boot
	    ;;
    *)
	    echo_with_prompt "No compatible OS found, Skipping installing java dependencies"
	    ;;
esac
