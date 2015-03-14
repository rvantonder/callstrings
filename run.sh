#!/bin/bash -e

echo "==== Compiling test binaries ===="
make -C src

echo "==== Generating symbol tables ===="
./gen-syms.sh bin/*

echo "==== Building callstrings ===="
make

echo "==== Running callstrings ===="
./callstrings.native bin/example1 bin/example1.scm 10 --dot

echo "==== Creating graph ===="
dot -Tpng 03.dot -o example1.png
rm 03.dot
echo "--> Done!"

echo "==== Executing tests===="
make test
