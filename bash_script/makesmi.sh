#/bin/bash

input=$1
output=${input%.*}".smi"

smlsharp < ${input} | tail -n +2 | sed 's/^#[ >]*\(.*\)/\1/g' | \
  sed 's/\[//g' | sed 's/\]//g' | sed 's/'\''[a-z]\.//g' |
  sed 's/= fn //g' | sed 's/= .* :/:/g'> ${output}
