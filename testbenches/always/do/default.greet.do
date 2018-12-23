#!/bin/sh -e

src=$(zedo find "${ZEDO_TARGET%.greet}.txt")
echo "Hello, $(cat $src)!" > $1
