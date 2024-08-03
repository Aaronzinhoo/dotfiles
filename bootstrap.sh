#!/bin/bash

. "$( pwd )/utils.sh"

PROMPT='[ Bootstrap ]: '

get_symlink_files(){
    find . -mindepth 1| grep -vE './.git/|\.gitignore|\.gitmodules|bootstrap_extensions|fonts|os|.*.md|.*\.sh|.*.emacs/|windows'
}

link () {
    symlink_files=($(get_symlink_files))
    for path in "${symlink_files[@]}";
    do
        # Silently ignore errors here because the files may already exist
        path=${path#./}
        if [ -d "$path" ]  && [ "$path" = "emacs" ]; then
            ln -fhs "$( pwd )/$path" "$EMACS_INSTALL_DIR"
        elif [ -f "$( pwd )/$path" ]; then
            ln -fhs "$( pwd )/$path" "$HOME"
        fi
    done
}

# TODO rewrite this to check for os=unknown, use the execute_func_with_prompt wrapper, etc
install_packages () {
    case $OSTYPE in
	    darwin*)
            echo_with_prompt "Detected OS macOS"
	        echo_with_prompt "This utility will install useful utilities using Homebrew"
	        echo_with_prompt "Proceed? (y/n): "
	        read resp
	        # TODO - regex here?
	        if [ "$resp" = 'y' ] || [ "$resp" = 'Y' ] ; then
	            echo_with_prompt "Installing useful stuff using brew. This may take a while..."
	            sh $( pwd )/package_managers/brew_packages.sh
	        else
		        echo_with_prompt "Brew installation cancelled by user"
	        fi
	        ;;

	    linux-gnu*)
            echo_with_prompt "Detected OS Linux"
	        echo_with_prompt "This utility will install useful utilities using apt (this has been tested on Debian buster)"
	        echo_with_prompt "Proceed? (y/n)"
	        read resp
	        # TODO - regex here?
	        if [ "$resp" = 'y' ] || [ "$resp" = 'Y' ] ; then
	            echo_with_prompt "Installing useful stuff using apt. This may take a while..."
	            sudo sh $( pwd )/package_managers/apt_packages.sh
	        else
		        echo_with_prompt "Apt installation cancelled by user"
	        fi
	        ;;
	    *)
	        echo "No compatible OS found, skipping package installaion"
	        ;;
    esac
}

execute_func_with_prompt link "symlink all needed files"
install_packages

echo_with_prompt "applying zsh bootstrap to installation; errors may be experienced for packages that have not been setup yet"
apply_bootstrap_extension "$( pwd )/bootstrap_extensions/zsh_bootstrap.sh"

for BOOTSTRAP in ./bootstrap_extensions/*; do
    if [ "$BOOTSTRAP" = "./bootstrap_extensions/zsh_bootstrap.sh" ]; then
	    continue
    fi
    echo_with_prompt "applying ${BOOTSTRAP} to installation"
    apply_bootstrap_extension $BOOTSTRAP
done

# Hack to make sure this script always exits successfully
# Since the user may choose to cancel a step here and that is cool
true
