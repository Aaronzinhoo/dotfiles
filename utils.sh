#!/usr/bin/env bash

import_package(){
    if [ -f "$1" ]; then
        . "$( pwd )/${1}"
    fi
}

check_and_mkdir(){
    if [ ! -d "$1" ]; then
        mkdir -p "$1"
    fi
}

apply_bootstrap_extension(){
    if [ -f "$1" ]; then
        "$1"
    else
        echo_with_prompt "failed to execute file ${1}."
    fi
}

echo_with_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinho:Dotfiles ]: }"
    echo "${PROMPT} $*"
}

check_system_package_installed() {
    # enter package name to verify it is installed
    command -v "$1" | grep -o "$1" > /dev/null &&  return 0 || return 1
}

execute_func_with_prompt() {
    # Args
    # $1 - the function to call
    # $2 - the thing this function does
    # Returns 1 if the user cancels the operation
    # Returns 2 if the function failed
    # Returns 0 if all went well

	echo_with_prompt "This utility will $2"
	echo_with_prompt "Proceed? (y/n)"
	read -r resp
    resp=$(echo "$resp" | tr '[:upper:]' '[:lower:]')
	# TODO - regex here?
	if [ "$resp" == 'y' ] ; then
        # This thing here "calls" the function
        $1 || return 2
		echo_with_prompt "$2 complete"
	else
		echo_with_prompt "$2 cancelled by user"
        return 1
	fi
}
# TODO determine if can delete
get_os() {
    local os=''
	if echo "$OSTYPE" | grep 'darwin'; then
        os='darwin'
    elif echo "$OSTYPE" | grep 'linux-gnu'; then
        # This file contains all the details you need!
        source /etc/os-release
        # Set os to ID_LIKE if this field exists
        # Else default to ID
        # ref. https://www.freedesktop.org/software/systemd/man/os-release.html#:~:text=The%20%2Fetc%2Fos%2Drelease,like%20shell%2Dcompatible%20variable%20assignments.
        os="${ID_LIKE:-$ID}"
    else
        os='unknown'
    fi
    # Also set an env var based on this
    echo "$os"
}
