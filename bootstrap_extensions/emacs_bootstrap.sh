. "$( pwd )/utils.sh"

PROMPT="[ EmacsExtensionLoader ]: "

if command -v emacs; then
    echo_with_prompt "Bootstrapping for emacs seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp      
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping emacs bootstrapping!"
        exit 0;
    fi
fi

echo_with_prompt "Installing Emacs";
brew tap d12frosted/emacs-plus
if command -v emacs &> /dev/null; then
    brew uninstall emacs-plus
fi
brew install emacs-plus@$EMACS_VERSION --with-xwidgets --with-imagemagick --with-native-comp --with-mailutils
