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
    local script err
    err=0
    ZEDO__log DEBUG "zedo_always"
    ZEDO__isRootInvocation || ZEDO__recordDep "$ZEDO__PARENT" "$ZEDO__TARGET"
    # TODO this is where ifchange/ifcreate would be tested
    ZEDO__clearFileInfo "$ZEDO__TARGET"
    ZEDO__recordFile '???' "$ZEDO__TARGET"
    script="$(ZEDO__searchForScripts "$ZEDO__TARGET")"
    if [ -z "$script" ]; then
        if [ -e "${ZEDO__srcDir}/${ZEDO__TARGET}" ]; then
            ZEDO__log TRACE "Source file available."
            ZEDO__TARGET_type=SRC
        else
            ZEDO__log ERROR "No source file or do script available for target: ${ZEDO__TARGET}"
            ZEDO__errorAccum="${ZEDO__errorAccum}
    ${ZEDO__TARGET}"
            return 0
        fi
    else
        ZEDO__log TRACE "Best available do-script is: ${script}"
        ZEDO__TARGET_script="$script"
        ZEDO__TARGET_type=OUT
    fi

    if [ "$ZEDO__TARGET_type" = SRC ]; then
        ZEDO__recordFile SRC "$ZEDO__TARGET"
        # FIXME only do the mkdir+cp if outDir != srcDir
        mkdir -p "$(dirname "${ZEDO__outDir}/${ZEDO__TARGET}")"
        cp "${ZEDO__srcDir}/${ZEDO__TARGET}" "${ZEDO__outDir}/${ZEDO__TARGET}"
    elif [ "$ZEDO__TARGET_type" = OUT ]; then
        ZEDO__recordFile OUT "$ZEDO__TARGET"
        echo >&2 "zedo always unimplemented for output files"
        # then run the script that was found
        # did the script work?
            # if so, move the resulting file into place
            # if not, clean up the output file
    fi # TODO throw in a programmer error on the else branch
    ZEDO__recordHash "${ZEDO__TARGET}"
    # record the hash
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
