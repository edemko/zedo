#!/bin/sh -e

zedo ifchange "$2.txt"
# NOTE: don't pass $src to any zedo functions
# zedo only takes target names, not files
# other commands will need files, so use `zedo find <targetName>` to retrieve the target file (source or output) of the named target
src=$(zedo find "$2.txt")
echo "Hello, $(cat $src)!" > $1
