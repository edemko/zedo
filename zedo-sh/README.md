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

### Architectural Notes

The code is organized into a layered architecture.
The entry point, including argument parsing and environment setup, is `zedo.sh`.
Outside of the entry point, files should only define variables and functions; variables should only be exported when directed by the entry point.
The core functionality is implemented in `zedo_funcs.sh`, with one function for each command.
Because zedo, when given multiple files, operates on each file individually, each core function only operates on a single file, and the entry point manages looping as part of dispatch.
The lowest layers are in `zedo_utils.sh`, and include a database abstraction, a logging and aborting system, basic constants and tests, and helper functions.
The main entry can use the utilities functions directly, though it should not use the database code directly.

To keep variable names from colliding, there are a number of prefixes.
Functions and constants in utilities start with `ZEDO__`.
Core functions start with `zedo_`.
While the zedo configuration is being worked out, those temporary variables start with `ZEDO__config_`.
Theoretically, `zedo_funcs.sh` could be deployed separately from the main script and used as a sort of api from sh/bash/&c, but I'm not sure it'd actually be useful, especially relative to the extra deployment complexity.
In any case, variables beginning with `ZEDO__` are meant to be used internally only.


Although the static architecture is not too complex, the dynamic architecture is quite delicate.
In particular, an invocation of zedo must pass information down to child invocations over a do-script, and all this information is highly stateful because it must use environment variables to do so.

Most of this information is invariant over the entire zedo invocation tree, so we only set it from the root invocation.
Therefore, the first step is to determine parentage information, which is also the most important step so that dependency information can be logged.
The idea is that, before a do-script is run, `ZEDO__TARGET` is set; when that do-script then invokes zedo recursively, what is currently in `ZEDO__TARGET` is actually the parent for this invocation, and so is moved to `ZEDO__PARENT`.
Once parentage has been determined, the `ZEDO__isRootInvocation` function will work, and can be used to guard actions which only occur during root invocation.

In the next step, available command-line options are (mostly) determined.
I expect that some options will only be available for certain commands, but due to the limitiations of `getopt` (or my knowledge of it), these cannot be parsed until after command dispatch.
It is at this stage that informational invocations (i.e. `--help` or `--version`) spew and exit.
Otherwise, various configuration variables are set, either finally, or as a `ZEDO__config_` variable for further processing later.

So far, the only `ZEDO__config_` variable is for `baseDir`, which may need to be determined later when not explicitly configured.
Importantly, if the command is zedo `init`, then `ZEDO__findBaseDir` should not be used, so that's why final configuration is delayed.

Finally, the entry point dispatches to code for each command.
Many commands check that the invocation is in the correct place in the invocation tree with `ZEDO__onlyInDoScript` or `ZEDO__notInDoScript`.
Then, any final command-line argument parsing is completed.
If the invocation is root, then all the derived environment variables are defined (by calling `ZEDO__setSandboxFromBase`), and if there may be child invocations all invariant environment variables are exported (by calling `ZEDO__exportConfigAndSandbox`) (aside from parentage, which was already exported).
It is at this point that the core functions from `zedo_func.sh` are called to perform actual work.
As the core functions execute, there may have been errors building targets; these errors are accumulated, logged, and the errorfulness is reported up the invocation tree before exit.

As mentioned earlier, most exported variables are invariant over the invocation tree, but there are two exceptions: `ZEDO__PARENT` and `ZEDO__TARGET`.
`ZEDO__PARENT` is computed at each invocation's start-up.
In contrast, `ZEDO__TARGET` can vary even within a single invocation; however, it remains invariant during each execution of a core function from `zedo_funcs.sh`.
Each command's dispatch code at the end of `zedo.sh` manages `ZEDO__TARGET` as necessary; i.e. the core functions do not set it up themselves.
Correspondingly, uses of `ZEDO__TARGET` (other than determining parentage) should only occur within core functions.
Importantly, many calls to record information in the database make use of (directly or indirectly) `ZEDO__TARGET`, and so should only be called from the core functions.


I was thinking to protect a sandbox from having two zedo processes operate simultaneously, but I'm not sure that it's a useful feature.
If I decide to support it in the future, the idea is to call `ZEDO__startZedo` at the start of most commands' dispatch, and correspondingly also call `ZEDO__stopZedo` at the end, but both guarded so that only the root invocation does it.
Additionally, all functions that implement errorful exits should call `ZEDO__stopZedo`, again guarded so as only to run during the root invocation.
This last place means that `ZEDO__die` and its ilk cannot be called until `ZEDO__isRootInvocation` will work.
For now, `ZEDO__{start,ztop}Zedo` are no-ops.

## Details

### Script Search Algorithm

Given a target file `path/to/foo.a.b.c`, look for `/path/to/foo.a.b.c.do` under the scripts directory.
If that doesn't work, look for `path/to/default.a.b.c.do`, `path/to/default.b.c.do`, `path/to/default.c.do`, and so on, until potential extensions have been exhausted (note: zedo never looks for `default.do`).
If that still doesn't find a script, go up a directory and repeat the search through extensions (i.e. `path/default.a.b.c.do`, `path/default.b.c.do`, &c).
Continue searching upwards until the script directory would be exited.

Extensions are largely just character sequences beginning with a dot continuing to the end of the filename starting from the first dot, but there are exceptions.
If the file starts with a dot (i.e. `.foo.bar`), that's assumed to be a marker for a hidden file, and is therefore skipped (i.e. the only possible extension is `.bar`, not `.foo.bar`).
If an extension would otherwise start with consecutive dots, only the last dot counts (i.e. `foo.bar..baz` has possible extensions `.bar..baz` and `.baz`, but not `..baz`).

### Target Name Normalization

When comparing target paths to determine equality, some normalization steps are applied.
Paths that begin with a slash are absolute as far as zedo is concerned: they are found on the filesystem by searching from the top of the relevant directory (source, build, or script).
Otherwise, the path is relative to the parent; where there is no parent, it is the same as absolute.
Sequences `//`, `./` and wherever possible `<foo>/..`, are ignored.
If the resulting path starts with a `../`, that is an error.
