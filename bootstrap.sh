#!/bin/bash

. "$( pwd )/utils.sh"

PROMPT='[ Bootstrap ]: '

PATH_TO_DEVELOPMENT="$HOME/development"
PATH_TO_ORG="$HOME/org"
EMACS_INSTALL_DIR="${HOME}/.emacs.d"

# Initialize a few things
init () {
    echo_with_prompt "Making a development folder in $PATH_TO_DEVELOPMENT if it doesn't already exist"
	mkdir -p "$PATH_TO_DEVELOPMENT"
	echo_with_prompt "Making an org folder in $PATH_TO_ORG if it doesn't already exist"
	mkdir -p "$PATH_TO_ORG"
}

# TODO : Delete symlinks to deleted files
# Is this where rsync shines?
# TODO - add support for -f and --force
link () {
    symlink_files=($(get_symlink_files))
    for file in "${symlink_files[@]}";
    do
        # Silently ignore errors here because the files may already exist
        file=${file#./}
        if [ -d "$file" ]  && [ "$file" = "emacs"  ]; then
            ln -s "$( pwd )/$file" "$EMACS_INSTALL_DIR"
        elif [ -f "$( pwd )/$file" ]; then
            ln -s "$( pwd )/$file" "$HOME"
        fi
    done
}

# TODO rewrite this to check for os=unknown, use the execute_func_with_prompt wrapper, etc
install_tools () {
    local os=$(get_os)
	if [ "$os" = 'darwin' ] ; then
        echo_with_prompt "Detected OS macOS"
		echo_with_prompt "This utility will install useful utilities using Homebrew"
		echo_with_prompt "Proceed? (y/n)"
		read resp
		# TODO - regex here?
		if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
			echo_with_prompt "Installing useful stuff using brew. This may take a while..."
			sh brew.exclude.sh
		else
			echo_with_prompt "Brew installation cancelled by user"
		fi
	else
		echo_with_prompt "Skipping installations using Homebrew because MacOS was not detected..."
	fi

	if [ "$os" = 'debian' ] ; then
        echo_with_prompt "Detected OS $os"
		echo_with_prompt "This utility will install useful utilities using apt (this has been tested on Debian buster)"
		echo_with_prompt "Proceed? (y/n)"
		read resp
		# TODO - regex here?
		if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
			echo_with_prompt "Installing useful stuff using apt. This may take a while..."
			sudo sh apt.sh
		else
			echo_with_prompt "Apt installation cancelled by user"
		fi
	else
		echo_with_prompt "Skipping installations using apt because Debian/Linux was not detected..."
	fi
}


init
execute_func_with_prompt link "symlink everything"
install_tools
for BOOTSTRAP in ./bootstrap_extensions/*
do
    echo_with_prompt "applying ${BOOTSTRAP} to installation"
    apply_bootstrap_extension $BOOTSTRAP
done

# Hack to make sure this script always exits successfully
# Since the user may choose to cancel a step here and that is cool
true
