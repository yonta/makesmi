#/bin/bash

outputname=${1%.sml}".smi"
tempname=${1%.sml}".tmp"

cat $@ > ${tempname}
smlsharp < ${tempname} | tail -n +2 | sed 's/^#[#> ]*\(.*\)/\1/g' | \
  sed 's/\[//g' | sed 's/\]//g' | sed 's/'\''[a-z]\.//g' |
  sed 's/= fn //g' | sed 's/= .* :/:/g' > ${outputname}
