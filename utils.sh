#!/usr/bin/env bash

echo_with_prompt () {
    # The narcissistic default prompt
    PROMPT="${PROMPT:-[ Aaronzinhoo:Dotfiles ]: }"
    echo "${PROMPT} $*"
}

execute_func_with_prompt() {
    # Args
    # $1 - the function to call
    # $2 - the thing this function does
    # Returns 1 if the user cancels the operation
    # Returns 2 if the function failed
    # Returns 0 if all went well

	echo_with_prompt "$2"
	echo_with_prompt "Proceed? (y/n): "
	read -r resp
    resp=$(echo "$resp" | tr '[:upper:]' '[:lower:]')
	# TODO - regex here?
	if [ "$resp" == 'y' ] ; then
        # This thing here "calls" the function
        $1 || return 2
		echo_with_prompt "$1 execution call complete"
	else
		echo_with_prompt "$1 execution canceled"
        return 1
	fi
}
