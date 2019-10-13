#!/bin/sh
set -e

### Load up zedo libraries ###

# TODO in the final form, don't do any sourcing
HERE="$(dirname "${0}")"
. "${HERE}/zedo_funcs.sh"

### Determine Parentage ###

# If this script was called with ZEDO__TARGET defined,
# then that must have been the target under build when this script was invoked.
# As such, we can determine that the parent of this invocation is in `ZEDO__TARGET`.
if [ -n "${ZEDO__TARGET}" ]; then
    export ZEDO__PARENT="${ZEDO__TARGET}"
    unset ZEDO__TARGET
else
# If there was no `ZEDO__TARGET` when this script was invoked,
# there must not be a parent, and we are root
    unset ZEDO__PARENT
fi
# Only after this point will `ZEDO__isRootInvocation` operate as expected





### Parse Options ###

# NOTE Variables starting with `ZEDO__config_` are only used when working out what the true
# variables that will be used for the process and all children will be.
# After that (i.e. after ZEDO__setSandboxFromBase), they should not be used or exported.


# whether this is a root or child invocation this will determine which options are available
if ZEDO__isRootInvocation; then
    OPTS="hqv"
    LONGOPTS="help,version,quiet,verbose,zedo-dir:"

    # initial values
    ZEDO__verbosity=1
    ZEDO__config_basedir=
else
    OPTS=""
    LONGOPTS=""
fi

ARGS=$(getopt -n "${0}" -o="${OPTS}" -l="${LONGOPTS}" -- "$@")
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
            ZEDO__verbosity=$((${ZEDO__verbosity} - 1))
            ;;
        -v|--verbose)
            ZEDO__verbosity=$((${ZEDO__verbosity} + 1))
            ;;
        --zedo-dir)
            if [ -n "${2}" ]; then
                ZEDO__config_basedir="${2}"
                shift
            fi
            ;;
        --)
            shift
            break
            ;;
        *) ZEDO__programmerError "Unimplemented option '${1}'" ;;
    esac
    shift
done





### Dispatch to Zedo Command ###

# determine command
case "${1}" in
    init|reset | always|ifcreate|ifchange | phony|touch)
        CMD="${1}"
        shift
        ;;
    developer-stuff) # NOTE this is an undocumented command used only for testing stuff out temporarily
        exit 0
        ;;
    *)
        CMD=always
        ;;
esac

# based on command, dispatch to zedo functions
case "${CMD}" in
    init)
        ZEDO__notInDoScript "${CMD}"
        if [ -n "${ZEDO__config_basedir}" ]; then
            INIT_DIR="${ZEDO__config_basedir}"
            if [ "$#" != "0" ]; then
                ZEDO__log WARNING "multiple directories given to ${0} ${CMD}; ignoring all but --zedo-dir"
            fi
        elif [ "$#" = "0" ]; then
            INIT_DIR=`pwd`
        elif [ "$#" = "1" ]; then
            INIT_DIR="${1}"
        else
            ZEDO__log WARNING "multiple directories given to ${0} ${CMD}; ignoring all but first"
            INIT_DIR="${1}"
        fi
        ZEDO__setSandboxFromBase "${INIT_DIR}"
        zedo_${CMD}
        ;;
    reset)
        ZEDO__notInDoScript "${CMD}"
        if [ "$#" != "0" ]; then
            ZEDO__log WARNING "unexpected arguments to ${0} ${CMD} found; ignoring extras"
        fi
        if [ -z "${ZEDO__config_basedir}" ]; then
            ZEDO__config_basedir="$(ZEDO__findBaseDir `pwd`)"
        fi
        ZEDO__setSandboxFromBase "${ZEDO__config_basedir}"
        zedo_${CMD}
        ;;
    always|ifchange|ifcreate)
        ZEDO__setSandboxFromBase "${ZEDO__config_basedir}"
        ZEDO__errorAccum=""
        for f in "$@"; do
            ZEDO__setTarget "$f"
            zedo_${CMD}
            ZEDO__unsetTarget
        done
        if [ -n "$ZEDO__errorAccum" ]; then
            ZEDO__die "Some targets failed to build.${ZEDO__errorAccum}"
        fi
        ;;
    # NOTE commands that might call zedo recursively must set&export sandbox
    *) ZEDO__programmerError "Unimplemented command '${CMD}'" ;;
esac
