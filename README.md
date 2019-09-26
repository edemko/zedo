# Zedo

A build system based on djb's redo, but with some limitations removed and a command line interface inspired by git.

With redo, the idea is to have a system-wide database; in zedo, you can (and must) create sandboxes.
In zedo, you can specify separate source, build script, and output directories.
In zedo, you can have multiple output files from a single do-script.
In zedo, the arguments passed to do-scripts are slightly different, accounting for some self-admitted weaknesses in djb's initial idea and generally making the interface nicer.

Zedo is implemented entirely in Bourne Shell, so that it has very few dependencies.
Other than the usual utilities that are generally expected in a \*nix distro, zedo depends on sqlite3 to implement a database for build-result and dependency information.


## Development

For now, `src/zedo.sh` is executable, and sources from `src/zedo_{utils,funcs}.sh`.
Once the project advances to the state where do-scripts will want to call zedo, I think that'll have to change.
Besides, I'd like to deploy zedo as single file when this is all done.

There is no automated test suite as of yet.
However, I am accumulating notes of what I've been manually testing during development in [`TEST.md`](TEST.md).

The `zedo_{utils,funcs}.sh` files only contain variable and function definitions, and none of the variables are exported.
All and only zedo must execute when invoked as a script is contained in `zedo.sh`.

The functions in `zedo_funcs.sh` implement the various zedo commands (init, ifchange, always), though if a command would operate on multiple files, the function only operates on a single file at a time.
Theoretically, these functions could be deployed separately from the main script and used as a sort of api from sh/bash/&c, but I'm not sure it'd actually be useful, especially relative to the extra deployment complexity.

The definitions in `zedo_utils.sh` are there to reduce code duplication.
They include:

  * constants that describe the zedo program itself
  * a small set of logging/verbosity-related/death functions
  * an abstraction layer isolating database manipulation from the rest of the system (currently using sqlite)
  * primitive, low-level operations used by `zedo_funcs.sh`
