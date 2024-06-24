#!/bin/bash

rm -rf ~/.npm
rm -rf $(pyenv root)
rm -rf `goenv root`
rm -rf "${XDG_CONFIG_HOME}/nvm"
rm -rf "${XDG_CONFIG_HOME}/sdkman"

[[ "$OSTYPE" =~ darwin* ]] && brew uninstall goenv && brew uninstall sdkman-cli
