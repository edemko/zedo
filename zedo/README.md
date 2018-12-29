# Zedo

Rebuild target files when dependencies change.

The software* you want to build may be very complex.
An automatic build system is just another piece of software, and should be treated as such.
If it isn't very complex, then a few build scripts should take care of it.
If it does get complex though, you'll want the comfort of a full scripting language to do with as you see fit.
Zedo gains its power by not taking anything in your project for granted; it merely coordinates your build scripts.

\*It doesn't need to be software, it could be anything stored digitally --- a blog, music, the results of scientific analysis, &c. That said, you'll be a little more comfortable writing build scripts if you're already a programmer.

If you are familiar with `redo`, `zedo` is based on that, but does not attempt to be backwards-compatible with DJB's original discussions.
Now that `redo` has been experimented with in practice, a few limitations of the system have been identified.
My goal with zedo is not to follow DJB's words as scripture, but to create the best tool I can, and that means altering the interface to correct the subtle design errors.


## Status

Zedo promises a lot, but it's not entirely implemented yet.
Below is an incomplete checklist of what features need to be done.
This is a detailed list because it's also serving as documentation about the algorithm and interface until real documentation can be written.

- [ ] startup
    - [x] check $ZEDO_TARGET exists: if so, this is a subprocess, not the root one
    - [x] move to zedo base dir
    - [ ] zedo command is always on the path
    - [ ] as root process
        - [ ] register self in the db as running
        - [ ] remove self from db when exiting
        - [ ] if another process got there first, exit
        - [x] clear data from previous builds (e.g. the old one might have died as a result of power failure)
    - [x] as sub-process
        - [x] find the base dir from $ZEDO__BASEDIR
- [ ] zedo init
    - [x] if no explicit dir given, use cwd
    - [x] create version descriptor file
    - [ ] create the database
        - [x] current_build table
        - [x] target table
        - [ ] dependencies table
        - [ ] extra files table
        - META: other tables
    - [x] do not (re-)initialize if another process is already using it
        - [x] register pid as soon as db created
        - [x] check for other pid and abort
    - [ ] re-initialize
- [ ] zedo find
    - [x] if target starts with `./` or `../`, then target is relative to parent target
    - [x] absolute paths to target may start with a slash
    - [x] enumerate all postentially-relevant files (absolute paths)
    - [x] by default print target file
    - [x] fail if neither script nor source exists
    - [ ] options to print other information, combinations of:
        - [ ] is source
        - [ ] do-script
        - [ ] search trail of do-scripts
        - [ ] dist file
        - [ ] known dependencies
        - [ ] detected extension
    - [ ] maybe cache it in the db, but then the db becomes a bottleneck for finding things, where I think the filesystem can work in parallel
- [ ] zedo always
    - [x] call out to the build procedure
    - [ ] parallelism
        - [ ] check if more that one job slot is available
        - [ ] call the build procedure for each
        - [ ] join all the sub-processes
    - [ ] flag to also print the target file
- META: other zedo commands
- [ ] zedo {phony,volative,also}
    - [x] phony set parent's phony flag in db
    - [ ] volative set parent's volative flag in db
    - META
- [ ] procedure: `build` on source file
    - [ ] clear the db
        - [ ] set state to "building"
        - [ ] clear extra actions
        - [ ] clear dependencies
        - [ ] delete hash from db
    - [ ] register as dependency of parent in db
    - [ ] cleanup
        - [ ] set state in db to "ok" or "fail" on success/fail
        - [ ] register hash in db if successful
- [ ] procedure: `build` on output file
    - [ ] if another process is building, wait for it to complete
    - [ ] clear the db
        - [ ] set state to "building"
        - [x] clear hash
        - [ ] clear dependencies
        - [x] clear phony and volatile flags
        - [ ] clear extra actions
    - [ ] register as dependency of parent in db
    - [ ] create locations
        - [x] create parent directory for out file
        - [x] create parent directory for the temp file&dir
        - [x] create temp file
        - [ ] create temp dir
        - [ ] create/truncate old log files
    - [ ] building
        - [x] set ZEDO_TARGET and ZEDO__BASEDIR
        - [x] create process that executes the build script
        - [x] `$1` is the temp file
        - [x] `$2` is the target name without extension
        - [x] stdin is closed
        - [ ] stdout and stderr go to the respective log files
    - [ ] cleanup
        - [x] move target files
            - [x] if failure, remove temp file
            - [x] if target is phony, do not move output file
            - [x] else move temp file to output file
        - [x] set state in db to "ok" or "fail" on success/fail
        - [ ] register hash in db if successful
            - [ ] except if volatile exists, do not hash
        - [ ] handle extra actions
            - [ ] if also-records exist:
                - [ ] record the script that built it in the db? (so a way can be found to build it even if it doesn't have a script)
                - META
    - META
- [ ] auditing
    - [ ] only use System.Exit routines in the main module
    - [ ] ensure all errors are handled by using only checked exceptions
    - [x] all file paths relative to zedo base dir
    - META
