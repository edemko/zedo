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
    to build a file: ${0} [always|ifchange|ifcreate] files...
    to mark self as a no-output target: ${0} phony
    to target add'l files: ${0} touch files...
    to smash a corrupted working state back into line: ${0} reset

OPTIONS
    -h, --help       Display help text and exit
    -q, --quiet      Decrease verbosity (default 1)
    -v, --verbose    Increse verbosity (default 1)
    --version        Display version number and exit
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

    ZEDO__baseDir="${1}"
    ZEDO__log TRACE "ZEDO__baseDir=${ZEDO__baseDir}"

    ZEDO__workDir="${ZEDO__baseDir}/.zedo"
    ZEDO__log TRACE "ZEDO__workDir=${ZEDO__workDir}"

    ZEDO__dbfile="${ZEDO__workDir}/build.sqlite"
    ZEDO__log TRACE "ZEDO__dbfile=${ZEDO__dbfile}"

}
ZEDO__exportConfigAndSandbox() {
    export ZEDO__verbosity
    export ZEDO__baseDir ZEDO__workDir ZEDO__dbfile
}

ZEDO__builddb() {
    ZEDO__log DEBUG "ZEDO__builddb"
    ZEDO__db "${ZEDO__schema}" || ZEDO__alreadyRunning
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





ZEDO__db() {
    ZEDO__log SQL "${1}"
    if ZEDO__levelIs DEBUG; then
        sqlite3 -batch "${ZEDO__dbfile}" "${1}"
    else
        2>/dev/null sqlite3 -batch "${ZEDO__dbfile}" "${2}"
    fi
}

ZEDO__schema="
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

CREATE TABLE files
    ( type              TEXT NOT NULL
    , path              TEXT NOT NULL
    , last_known_hash   TEXT
    );
"

ZEDO__startup_sql="INSERT INTO status (key, val) VALUES ('root_pid', $$);"
ZEDO__teardown_sql="DELETE FROM status WHERE key = 'root_pid';"
