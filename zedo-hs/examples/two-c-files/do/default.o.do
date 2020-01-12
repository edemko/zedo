#!/bin/sh -e

CC=`zedo ifchange -f cc`
cfile=`zedo ifchange -f "${2}.c"`

echo >&2 "COMPILING $cfile"
$CC "$1" "$cfile"

