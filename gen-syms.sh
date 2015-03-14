#!/bin/bash -e
# ./gen-syms.sh <binary file>

script="from idautils import *
with open('%s.scm', 'w+') as out:
    for ea in Segments():
        fs = Functions(SegStart(ea), SegEnd(ea))
        for f in fs:
            out.write ('(%%s 0x%%x 0x%%x)\\\n' %% (
                GetFunctionName(f),
                GetFunctionAttr(f, FUNCATTR_START),
                GetFunctionAttr(f, FUNCATTR_END)))
idc.Exit(0)"

for i in "${@:1}"; do
  if [[ $i == *.scm ]] || [[ $i == *.idb ]]; then
    continue
  fi

  echo "--> Creating symbols for $i"
  printf "$script\n" "${i%.*}" > script.py
  idal -A -Sscript.py "$i"

  rm script.py
  rm "${i%.*}.idb"
done
