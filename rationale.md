Something a sqlite db will give me is locking for free, so I don't have to do lots of file locks.
Unfortunately, it will be a single lock over which all jobs will contend: i.e. bottleneck.
Reducing contention is going to require serious design, though.



Files are put into source, output, and script locations.
What I'd like to support is multiple simultaneous builds.
Of course, one could just specify the multiple outputs by target filename, but then scripts (esp. final binaries) will likely need to be duplicated for each output.
If the output directory is specified by a zedo option instead of being reimplemented ad-hoc by each user, then the scripts can be shared by all the output directories.

An additional location I might like to have is `dist`.
Perhaps scripts can mark their output(s) as being distributed with the final release.
That is, there'd be a `zedo dist` that operates similarly to `zedo phony` that marks its output(s) for being copied to a dist location after the build is complete.

Having multiple script locations means that it is trivial to "import" a set of stock scripts.


As for logging, I'm not sure I like duplicating all the logs into their parents; that's exponential.
Instead, each do-script should get its own log, and any traces about how zedo itself is scheduling/configuring/executing should have its own (master?) log.
In my previous version, trying to pass file descriptors correctly between processes was a pain.


I don't need a daemon exactly, nor do I really need a centralized scheduler.
The {now,max}_jobs columns in the db are enough.
Say redo is called from a do-script; we're thinking about the code executing that command.
It knows that one of the jobs is itself (it's parent is waiting).
So, it can at least do its thing sequentially.
If there's some space for more jobs though (now_jobs < max_jobs), then it can spawn up to the difference to bring now_jobs up to max.
Each extra process that finishes needs to decrement now_jobs.
However, one of the redo jobs will return to the parent, so it needs to not decrement.
I guess this is what is called cooperative multitasking.


It'd be nice to also support redo-style scripts.
Let's say that the extension will distinguish stuff: `.zdo` for zedo and `.do` for plain redo.
The script gets called with different pipes and arguments depending only on the extension.
Of course, there's no way something with scripts in a separate directory will be compatible with redo, so if any of src-, out-, and scriptsDir are different from the others, I'll just use the zedo-style invocation.

In modern filesystems, extensions are really just a convention, not a cold hard rule.
Sure it might be easy like `foo.c`, but there might be no extension `foo`, two extensions `foo.tar.gz`, or just weird stuff like `.foo.a.b...c.` which, quite frankly, not many pieces of software are prepared to deal with.


If (relevant) environment variables change, things might need to be re-done.
I suppose there should be a way to declare a dependency on such a variable the same as we declare it on files.


I don't want to always have to print the absolute path.
Perhaps I should `cd` into the basedir, and then relativize files from there.
That'd make the zedo dir more movable as well, since only the part of the path inside the base dir is relevant during comparisons.
One thing I'll have to make sure of is that the `zedo` is always in $PATH so it can be called plain from do-scripts, whether installed or not.


There are sometimes scripts that I have to put in the source directory because they need to be tracked as a dependency.
It'd be better if I could put these in the do-script directory.




* `zedo init`
* `zedo find`
    By default, print the location of the target on stdout and exit.
    Pass options to configure the output (e.g. do-script search)
* `zedo {,always}`
* `zedo {if,}change{,d}`
* `zedo {if,}create{,d}`
* `zedo volatile` because this target must always be re-generated
* `zedo phony`
* `zedo also`
* `zedo clean`
* `zedo watch`
* `zedo ood` report out-of-date
* `zedo query` what are the known output/dist files, are there any output files without a 
discoverable script (i.e. made only though `zedo also`), and other db-level things
* further commands will simply abstract database queries, but those will only be needed in the final, polished product
* You know, sometimes, a glob is a useful thing to save in the if-change/if-create database

I find myself doing `a=$2.foo; zedo ifchange $a; a=$(zedo find $a)` a lot.
It'd be nice to do just `a=$(zedo ifchange --find $2.foo)`.
It'd actually also be nice if `zedo find` worked on multiple targets, with each result output on a new line so it can be used with `read`.