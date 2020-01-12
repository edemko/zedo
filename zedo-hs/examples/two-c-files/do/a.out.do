#!/bin/sh -e

ofiles=`zedo ifchange -f foo.o bar.o`
echo >&2 "LINKING ${ofiles}"
gcc $ofiles -o $1

