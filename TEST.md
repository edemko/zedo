# Zedo Testing Procedures

Because I am too lazy to automate my testing at the moment,
this file contains instructions about what sort of properties I've been on the lookout for during my manual testing.
It is broken up into sections based on the command begin tested


## General

Parentage:

  * there is a parent only parent when ZEDO__TARGET is set on startup
  * if there is a parent, ZEDO__TARGET is set to it on startup, but by the time commands are being executed, ZEDO__PARENT set to hold it and ZEDO__TARGET has different semantics (is different, except when a script is (likely) pathological)

Searching for a sandbox:

  * if the pwd contains a `.zedo` directory, report it
  * when a parent directory contains a `.zedo` directory, report the closest
  * when no parent directory contains a `.zedo`, abort
  * [TODO] also abort if closest sandbox is on a different filesystem

Boring stuff:

  * -v and -q flags (eqiv. --verbose, --quiet) inc/decrement verbosity, thereby adjusting error levels
  * default verbosity level is 1
  * -h (equiv. --help) prints usage
  * usage text actually matches real usage
  * --version behaves exactly as said on the tin

## zedo init

Configuration:

  * if an argument is given to init, put the .zedo dir inside that argument;
    otherwise, use the present working directory

Postconditions:

  * the workDir must be a directory
  * the dbfile must be a sqlite file
  * the dbfile schema should be as expected
  * the status table should not have a row under key = 'root_pid'

Minor stuff: 

  * if the workDir already exists, note that re-initialization is occuring
  * if the workDir does not already exist, note when initialization is complete

Notes:

  * at the moment, init does not lock out other zedo processes

## zedo reset

Preconditions:

  * a zedo sandbox must be found

Postconditions:

  * the status table should not have a row under key = 'root_pid'
  * all configuration files in the workDir should be left unchanged
