. "$( pwd )/utils.sh"

PROMPT="[ EmacsExtensionLoader ]: "

install_bootstrap_check "command -v emacs > /dev/null" "emacs" || exit 0;

EMACS_LSP_BOOSTER_VERSION="v0.2.1"

echo_with_prompt "Installing Emacs";
brew tap d12frosted/emacs-plus
if command -v emacs &> /dev/null; then
    brew uninstall emacs-plus
fi
brew install emacs-plus@$EMACS_VERSION --with-xwidgets --with-imagemagick --with-native-comp --with-mailutils

curl -L -s -o emacs-lsp-booster.zip "https://github.com/blahgeek/emacs-lsp-booster/releases/download/${EMACS_LSP_BOOSTER_VERSION}/emacs-lsp-booster_${EMACS_LSP_BOOSTER_VERSION}_x86_64-apple-darwin.zip"
unzip emacs-lsp-booster.zip
mv emacs-lsp-booster "$HOME/.local/bin"
rm emacs-lsp-booster.zip
