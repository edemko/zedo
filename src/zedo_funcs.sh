# TODO in the final form, don't do any sourcing
HERE=$(dirname "${0}")
. "${HERE}/zedo_utils.sh"


zedo_init() {
    ZEDO__log DEBUG "zedo_init"

    if [ -d "${ZEDO__workDir}" ]; then
        ZEDO__log '' "Re-initializing existing zedo project in ${ZEDO__workDir}"
    elif [ -e "${ZEDO__workDir}" ]; then
        ZEDO__die "Reserved path already exists: ${ZEDO__workDir}"
    else
        mkdir "${ZEDO__workDir}"
    fi

    if [ -e "${ZEDO__dbfile}" ]; then
        rm "${ZEDO__dbfile}"
    fi
    ZEDO__builddb
    # ZEDO__stopZedo # NOTE init doesn't currently register itself as running
    ZEDO__log '' "Initialized zedo sandbox in ${ZEDO__workDir}"
}

zedo_reset() {
    ZEDO__log DEBUG "zedo_reset"
    ZEDO__stopZedo
}

zedo_always() {
    echo >&2 "zedo_always unimplemented"
}
zedo_ifcreate() {
    echo >&2 "zedo_ifcreate unimplemented"
}
zedo_ifchange() {
    echo >&2 "zedo_ifchange unimplemented"
}

zedo_phony() {
    echo >&2 "zedo_phony unimplemented"
}
zedo_touch() {
    # find an output filepath relative to this scripts output
    # generate a temp file for that target
    # register the temp file as something to be moved into place as the target file once the parent script is done
    # print just the name of a temp file to stdout
    echo >&2 "zedo_touch unimplemented"
}

# TODO can this functionality be subsumed by zedo_touch?
# zedo_find() {
#     echo >&2 "zedo_find unimplemented"
# }
