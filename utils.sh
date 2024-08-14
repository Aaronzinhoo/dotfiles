#!/usr/bin/env bash

color_blue() {
  echo -e "\033[34m${1}\033[0m"
}

color_green() {
  echo -e "\033[32m${1}\033[0m"
}

color_yellow() {
  echo -e "\033[33m${1}\033[0m"
}

color_red() {
  echo -e "\033[31m${1}\033[0m"
}

echo_with_no_newline_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
    echo -ne "${PROMPT} $*"
}

echo_with_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
    color_blue "${PROMPT} $*"
}

echo_with_green_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
    color_green "${PROMPT} $*"
}

echo_with_yellow_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
    color_yellow "${PROMPT} $*"
}

echo_with_red_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
    color_red "${PROMPT} $*"
}

execute_func_with_prompt() {
    # Args
    # $1 - the function to call
    # $2 - the thing this function does
    # Returns 1 if the user cancels the operation
    # Returns 2 if the function failed
    # Returns 0 if all went well

	echo_with_prompt "$2"
	echo_with_no_newline_prompt "Proceed? (y/n): "
    local response
	read -r response
    response=$(echo "$response" | tr '[:upper:]' '[:lower:]')
	if [ "$response" == 'y' ] ; then
        # This thing here "calls" the function
        $1 || return 2
		echo_with_green_prompt "$1 execution call complete"
	else
		echo_with_yellow_prompt "$1 execution canceled"
        return 1
	fi
}

install_bootstrap_check() {
    # Args
    # $1 - the function to check if confirmation needed
    # $2 - the extension that is being bootstrapped
    # Returns 0 if all went well
    # Returns 1 if the user cancels the operation
    CONDITION="$1"
    EXTENSION_NAME="$2"
    if eval "$CONDITION"; then
        echo_with_prompt "Bootstrapping for $EXTENSION_NAME seems to be complete already."
        echo_with_no_newline_prompt "Do you wish to proceed with the bootstrap process? (y/n): "
        local response
        read -r response
        response=$(echo "$response" | tr '[:upper:]' '[:lower:]')
        if [ ! "$response" == 'y' ]; then
	        echo_with_yellow_prompt "Skipping $EXTENSION_NAME bootstrapping!"
            return 1;
        fi
    fi
}
