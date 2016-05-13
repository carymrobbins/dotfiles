#!/bin/bash

text_bold="$(tput bold)"
text_reset="$(tput sgr0)"
text_red="$(tput setaf 1)"
text_yellow="$(tput setaf 3)"
text_magenta="$(tput setaf 5)"

# Executes a command and puts each line into an array
# Usage: $ lines_to_array dirty_files \
#             git diff-files --name-only
lines_to_array() {
  local varname="$1"
  shift
  eval "${varname}=()"
  while IFS= read -r -d $'\n'; do
    eval "${varname}+=(\"$REPLY\")"
  done < <("$@")
}

# Joins an array with newlines into a string.
# Usage: $ array_to_lines my_array | sort
array_to_lines() {
  local varname="$1"
  eval "printf '%s\n' \"\${${varname}[@]}\""
}

# Returns elements that exist in both arrays.
# Usage: $ array_intersection array1 array2
array_intersection() {
  comm -12 <(array_to_lines "$1" | sort) <(array_to_lines "$2" | sort)
}
