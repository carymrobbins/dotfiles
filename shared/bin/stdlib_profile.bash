#!/bin/bash

profile() {
  local size=10
  local scale=3
  local unit
  local abort_on_error
  while true; do
    case "$1" in
      -e|--error) abort_on_error=1; shift;;
      -n|--size)  size=$2;  shift; shift;;
      -p|--scale) scale=$2; shift; shift;;
      -u|--unit)  unit=$2;  shift; shift;;
      --) shift; break;;
      *) break;;
    esac
  done
  # Prefix unit with space for easy %s formatting, if present.
  [ -z "$unit" ] || unit=" $unit"
  local sample
  local sample_sum=0
  for ((i=1; i <= size; ++i)); do
    sample=$("$@")
    exitcode=$?
    if [ $exitcode -ne 0 -a -n "$abort_on_error" ]; then
      return $exitcode
    fi
    sample_sum=$(bc <<< "$sample_sum + $sample")
    printf 'run %02d: %.3f%s\n' "$i" "$sample" "$unit"
  done
  if [ $size -ne 1 ]; then
    echo '-------------------'
    printf '  mean: %.3f%s\n' "$(bc <<< "scale=$scale; $sample_sum / $size")" "$unit"
  fi
}
