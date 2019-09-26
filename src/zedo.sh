#!/bin/sh
set -e

### Load up zedo libraries ###

HERE="$(dirname "${0}")"
# TODO I may "compile" (cat) all the scripts together for deployment, we'll see
. "${HERE}/zedo_funcs.sh" # FIXME have some error handling if you can't find the file

### Determine Parentage ###

# If this script was called with ZEDO__TARGET defined,
# then that must have been the target under build when this script was invoked.
# As such, we can determine that the parent of this invocation is in `ZEDO__TARGET`.
if [ -n "${ZEDO__TARGET}" ]; then
    ZEDO__PARENT="${ZEDO__TARGET}"
    unset ZEDO__TARGET
else
# If there was no `ZEDO__TARGET` when this script was invoked,
# there must not be a parent, and we are root
    unset ZEDO__PARENT
fi
# Only after this point will `ZEDO__isRootInvocation` operate as expected





### Parse Options ###

# initial values
ZEDO__VERBOSITY=1

# whether this is a root or child invocation this will determine which options are available
if ZEDO__isRootInvocation; then
    OPTS="hqv"
    LONGOPTS="help,version,quiet,verbose"
else
    OPTS=""
    LONGOPTS=""
fi

ARGS=$(getopt -n "${0}" -o="${OPTS}" -l="${LONGOPTS}" -- $@)
eval set -- $ARGS
while true; do
    case "${1}" in
        -h|--help)
            echo "${ZEDO__USAGE}"
            exit 0
            ;;
        --version)
            echo "${ZEDO__VERSION}"
            exit 0
            ;;
        -q|--quiet)
            ZEDO__VERBOSITY=$((${ZEDO__VERBOSITY} - 1))
            ;;
        -v|--verbose)
            ZEDO__VERBOSITY=$((${ZEDO__VERBOSITY} + 1))
            ;;
        --)
            shift
            break
            ;;
        *) ZEDO__programmerError "Unimplemented option '${1}'" ;;
    esac
    shift
done

# inherited configuration
export ZEDO__VERBOSITY





### Dispatch to Zedo Command ###

# determine command
case "${1}" in
    init|reset | always|ifcreate|ifchange | phony|touch)
        CMD="${1}"
        shift
        ;;
    *)
        CMD=always
        ;;
esac

# based on command, dispatch to zedo functions
case "${CMD}" in
    init)
        if [ "$#" = "0" ]; then
            INIT_DIR=`pwd`
        elif [ "$#" = "1" ]; then
            INIT_DIR="${1}"
        else
            ZEDO__log WARNING "multiple directories given to ${0} ${CMD}; ignoring all but first"
            INIT_DIR="${1}"
        fi
        ZEDO__setDirsFromBase "${INIT_DIR}"
        zedo_${CMD}
        ;;
    reset)
        # FIXME find the base dir
        if [ "$#" != "0" ]; then
            ZEDO__log WARNING "unexpected arguments to ${0} ${CMD} found; ignoring extras"
        fi
        ZEDO__setDirsFromBase "${ZEDO__BASEDIR}"
        zedo_${CMD}
        ;;
    *) ZEDO__programmerError "Unimplemented command '${CMD}'" ;;
esac
