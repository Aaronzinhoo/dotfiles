. "$( pwd )/utils.sh"

PROMPT="[ EmacsExtensionLoader ]: "

install_bootstrap_check "command -v emacs > /dev/null" "emacs" || exit 0;

echo_with_prompt "Installing Emacs";
brew tap d12frosted/emacs-plus
if command -v emacs &> /dev/null; then
    brew uninstall emacs-plus
fi
brew install emacs-plus@$EMACS_VERSION --with-xwidgets --with-imagemagick --with-native-comp --with-mailutils
