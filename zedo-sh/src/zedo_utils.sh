# These are constants and helper functions specific to the various zedo utilities.
# Every definition in here should start with `ZEDO__` to avoid collisions with other
# definitions in the environment.

# Most of the functions in here won't work until after ZEDO__setSandboxFromBase is called.
# These include: ZEDO__{builddb,startZedo,stopZedo} and ZEDO__db

ZEDO__VERSION_MAJOR=1
ZEDO__VERSION_MINOR=0
ZEDO__VERSION_PATCH=0
ZEDO__VERSION="${ZEDO__VERSION_MAJOR}.${ZEDO__VERSION_MINOR}.${ZEDO__VERSION_PATCH}a"
ZEDO__DBVERSION="0.0.0"

ZEDO__USAGE="Usage:
    to set up a project: ${0} init [directory]
                         ${0} --zedo-dir <directory> init
    to build a file: ${0} [always|ifchange|ifcreate] files...
    to mark self as a no-output target: ${0} phony
    to target add'l files: ${0} touch files...
    to smash a corrupted working state back into line: ${0} reset

OPTIONS
    -h, --help           Display help text and exit
    -q, --quiet          Decrease verbosity (default 1)
    -v, --verbose        Increse verbosity (default 1)
    --version            Display version number and exit
    --zedo-dir <path>    Operate inside the passed zedo sandbox (default \`pwd\`)
"

ZEDO__isRootInvocation() {
    [ -z "${ZEDO__PARENT}" ] && return 0 || return 1
}
ZEDO__onlyInDoScript() {
    local cmd="${1}"
    ZEDO__isRootInvocation && ZEDO__die "Do not call ${0} ${cmd} from a do-script."
    return 0
}
ZEDO__notInDoScript() {
    local cmd="${1}"
    ZEDO__isRootInvocation || ZEDO__die "Do not call ${0} ${cmd} from a do-script."
    return 0
}

ZEDO__findBaseDir() {
    ZEDO__log DEBUG "ZEDO__findBaseDir"
    local candidate
    candidate="${1}"

    while [ -n "${candidate}" -a "${candidate}" != "/" ]; do
        # TODO if it's not the same filesystem, abort
        ZEDO__log DEBUG "looking for baseDir: ${candidate}"
        if [ -d "${candidate}/.zedo" ]; then
            ZEDO__log DEBUG "found baseDir: ${candidate}"
            echo "${candidate}"
            return 0
        else
            candidate="$(dirname "${candidate}")"
        fi
    done
    ZEDO__die "Could not find zedo sandbox starting from ${1}."
}

ZEDO__setSandboxFromBase() {
    ZEDO__log DEBUG "ZEDO__setSandboxFromBase"

    ZEDO__baseDir="${1%/}" # delete traling slash
    ZEDO__log TRACE "ZEDO__baseDir=${ZEDO__baseDir}"

    ZEDO__workDir="${ZEDO__baseDir}/.zedo"
    ZEDO__log TRACE "ZEDO__workDir=${ZEDO__workDir}"

    ZEDO__dbfile="${ZEDO__workDir}/build.sqlite"
    ZEDO__log TRACE "ZEDO__dbfile=${ZEDO__dbfile}"

    ZEDO__srcDir="${ZEDO__baseDir}/src" # TODO allow configuration
    ZEDO__log TRACE "ZEDO__srcDir=${ZEDO__srcDir}"
    ZEDO__outDir="${ZEDO__baseDir}/build" # TODO allow configuration
    ZEDO__log TRACE "ZEDO__outdir=${ZEDO__outdir}"
    ZEDO__scriptDir="${ZEDO__baseDir}/do" # TODO allow configuration
    ZEDO__log TRACE "ZEDO__scriptDir=${ZEDO__scriptDir}"

    ZEDO_HASHALG=sha512 # TODO allow configuration

}
ZEDO__exportConfigAndSandbox() {
    export ZEDO__verbosity
    export ZEDO__baseDir ZEDO__workDir ZEDO__dbfile
}
ZEDO__setTarget() {
    ZEDO__TARGET="$1"
    if echo "$ZEDO__TARGET" | grep -q '^/'; then
        ZEDO__TARGET="${ZEDO__TARGET#/}" # delete leading slash
    elif [ -z "$ZEDO__PARENT" ]; then
        ZEDO__TARGET="$ZEDO__TARGET"
    else
        ZEDO__TARGET="$(dirname "$ZEDO__PARENT")/${ZEDO__TARGET}"
    fi
    # remove redundant parts of the path
    ZEDO__TARGET="$(echo "$ZEDO__TARGET" \
        | sed 's;//;/;g' \
        | sed 's;\(^\|/\)\./;\1;g' \
        | sed 's;\(^\|/\)[^/]\+/\.\./;\1;g' \
        )"
    # FIXME if the path still starts with a double-dot, that's a problem
    ZEDO__log TRACE "ZEDO__TARGET=${ZEDO__TARGET}"
}
ZEDO__unsetTarget() {
    unset ZEDO__TARGET
    unset ZEDO__TARGET_type
    unset ZEDO__TARGET_script
}

ZEDO__searchForScripts() {
    local target targetdir ext0 ext
    target="${1}"

    # look for the non-default script
    ZEDO__searchForScript "${target}.do" && return 0

    # look for default scripts
    targetdir="$(dirname "$target")/"
    ext0="$(basename "$target" | sed 's/^\.\?[^.]*//')"
    # traverse filesystem upwards, preferring deeper directories
    while true; do
        if [ "$targetdir" = "./" ]; then
            targetdir=''
        fi
        # cycle through all possible extensions, preferring longer ones
        ext="$ext0"
        while [ -n "$ext" ]; do
            # look for default.<ext>.do
            ZEDO__searchForScript "${targetdir}default${ext}.do" && return 0
            ext="$(echo "$ext" | sed 's/^\.[^.]*//' | sed 's/^\.\+/./')"
        done

        if [ "$targetdir" = '' ]; then
            break
        fi
        targetdir="$(dirname "$targetdir")/"
    done
}
ZEDO__searchForScript() {
    ZEDO__log DEBUG "ZEDO__searchForScript"
    local candidate
    candidate="$1"
    ZEDO__log TRACE "looking for do-script: ${candidate}"
    ZEDO__recordFile SCRIPT "$candidate"
    ZEDO__recordHash "$candidate"
    ZEDO__recordDep "$target" "$candidate" # FIXME probly need to record IFCREATE or IFCHANGE in dep table
    if [ -e "${ZEDO__scriptDir}/$candidate" ]; then
        echo "${candidate}"
        return 0
    else
        return 1
    fi
}


ZEDO__builddb() {
    ZEDO__log DEBUG "ZEDO__builddb"
    ZEDO__db "${ZEDO__schema}" # || ZEDO__alreadyRunning
}

ZEDO__startZedo() {
    ZEDO__log DEBUG "ZEDO__startZedo"
    # ZEDO__db "${ZEDO__startup_sql}" || ZEDO__alreadyRunning
    # if ZEDO__levelIs DEBUG; then
    #     ZEDO__db "SELECT * from status;"
    # fi
}
ZEDO__stopZedo() {
    ZEDO__log DEBUG "ZEDO__stopZedo"
    # ZEDO__db "${ZEDO__teardown_sql}"
    # if ZEDO__levelIs DEBUG; then
    #     ZEDO__db "SELECT * from status;"
    # fi
}




ZEDO__typeToLevel() {
    local type level
    type="${1}"
    case "${type}" in
        SQL) level=5 ;;
        DEBUG) level=4 ;;
        TRACE) level=3 ;;
        INFO) level=2 ;;
        NOTE|WARNING) level=1 ;;
        ERROR|FATAL) level=0 ;;
        *) level=0 ;;
    esac
    echo "${level}"
}
ZEDO__levelIs() {
    local type level
    type="${1}"
    level=`ZEDO__typeToLevel "${type}"`
    [ "${ZEDO__verbosity}" -ge "${level}" ] && return 0 || return 1
}

ZEDO__log() {
    local type msg prefix
    type=$1
    shift
    msg="$@"
    if [ -z "${type}" ]; then
        prefix=''
    else
        prefix="[${type}] "
    fi

    if ZEDO__levelIs "${type}"; then
        echo >&2 "${prefix}${msg}"
    fi
}
ZEDO__die() {
    ZEDO__log 'FATAL' $@
    if ZEDO__isRootInvocation && [ -n "${ZEDO__dbfile}" ]; then
        ZEDO__stopZedo
    fi
    exit 1
}

ZEDO__alreadyRunning() {
    ZEDO__log ERROR "It looks like another zedo process is running or didn't clean up after itself."
    ZEDO__log NOTE "Try \`zedo reset\` if you're sure another zedo instance isn't working on this project."
    ZEDO__die "Was not able to startup zedo."
}

ZEDO__programmerError() {
    echo >&2 "[PROGRAMMER ERROR] ${1}. Please file a bug report/patch!"
    echo >&2 "[WARNING] For now, cowardly aborting."
    if ZEDO__isRootInvocation && [ -n "${ZEDO__dbfile}" ]; then
        ZEDO__stopZedo
    fi
    exit 127
}




ZEDO__SQLARGS="-batch -bail -noheader -list -separator '|'"
ZEDO__db() {
    ZEDO__log SQL "${1}"
    if ZEDO__levelIs DEBUG; then
        sqlite3 $ZEDO__SQLARGS "${ZEDO__dbfile}" "${1}"
    else
        2>/dev/null sqlite3 $ZEDO__SQLARGS "${ZEDO__dbfile}" "${1}"
    fi
}

ZEDO__schema="
PRAGMA foreign_keys=1;

-- this table should have exactly one row at all times
CREATE TABLE version
    ( zedo_version   TEXT NOT NULL
    , db_version     TEXT NOT NULL
    );
INSERT INTO version (zedo_version, db_version) VALUES ('${ZEDO__VERSION}', '${ZEDO__DBVERSION}');

-- this table should have exactly one row at all times
CREATE TABLE status
    ( key       TEXT PRIMARY KEY
    , val       TEXT
    );

CREATE TABLE file
    ( type              TEXT NOT NULL DEFAULT('???')
    , path              TEXT PRIMARY KEY
    , last_known_hash   TEXT
    , isPhony           BOOL NOT NULL DEFAULT(0)

    , CONSTRAINT enum_type CHECK (type IN ('???', 'SRC', 'OUT', 'SCRIPT'))
    );

CREATE TABLE dep
    ( parent    TEXT NOT NULL REFERENCES file(path)
    , child     TEXT NOT NULL REFERENCES file(path)
    );

CREATE TABLE touch
    ( parent    TEXT NOT NULL REFERENCES file(path)
    , child     TEXT NOT NULL
    );
"

ZEDO__startup_sql="INSERT INTO status (key, val) VALUES ('root_pid', $$);"
ZEDO__teardown_sql="DELETE FROM status WHERE key = 'root_pid';"

ZEDO__recordFile() {
    # FIXME protect against sql injections in the parent and child filepaths
    local type target
    type="$1"
    target="$2"
    ZEDO__db "INSERT OR IGNORE INTO file (path) VALUES ('${target}');
UPDATE file SET type = '${type}' WHERE path = '${target}';"
}

ZEDO__recordDep() {
    # FIXME protect against sql injections in the parent and child filepaths
    local parent child
    parent="$1"
    child="$2"
    ZEDO__db "INSERT INTO dep (parent, child) VALUES ('${parent}', '${child}');"
}
ZEDO__recordHash() {
    ZEDO__log TRACE "ZEDO__recordHash"
    local path fullpath hash
    path="$1"
    type=$(ZEDO__db "SELECT type FROM file WHERE path = '${path}';")
    case "$type" in
        SRC) fullpath="${ZEDO__srcDir}/${path}" ;;
        OUT) fullpath="${ZEDO__outDir}/${path}" ;;
        SCRIPT) fullpath="${ZEDO__scriptDir}/${path}" ;;
        *) ZEDO__programmerError "bad type recorded in db ($type)" ;;
    esac

    if [ -e "${fullpath}" ]; then
        hash="$("${ZEDO_HASHALG}sum" "$fullpath")"
        hash="${ZEDO_HASHALG}:${hash%% *}"
        hash="'${hash}'"
    else
        hash=NULL
    fi
    ZEDO__db "UPDATE file SET last_known_hash = ${hash} WHERE path = '${path}';"
}

ZEDO__clearFileInfo() {
    # FIXME protect against sql injections in the target filepath
    local target
    target="$1"
    ZEDO__db "DELETE FROM dep WHERE parent = '${target}';
DELETE FROM touch WHERE parent = '${target}';
DELETE FROM file WHERE path = '${target}';"
}
