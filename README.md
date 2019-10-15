# zedo

Rebuild artifacts when dependencies change.

## The Story

So you have a file `<dependency>` and you need to create a file `<artifact>` from it.
You know the command(s) to build `<artifact>` on the command-line, but it's a pain to write that command every time.
So, you put the command into a script called `<artifact.do>`.

Perhaps `<dependency>` itself needs to be built before `<artifact>`.
You've already written a build script, `<dependency.do>`.
So, a step in `<artifact.do>` will be to invoke `<dependency.do>`.

Maybe a lot of scripts are the same except for a filename, so you add some extra arguments to the scripts so that you can use a single script for many things.
Usually, when scripts can be shared, the artifacts all end in the same extension.
So, let's make a convention and put reusable scripts in `default.<extension>.do`.
Just to make things easier on ourselves, let's make sure that every script is always called with the same argument structure; that way it's relatively easy to turn a specific `*.do` script into a generic `default.*.do` script.
Unfortunately, we've now broken the obvious correspondence between 
We'd have to search for the each do-script before we can invoke it
The question is: where do we put the default do-script, and then how do we search for it?

Depending on how you write you `*.do` scripts, you might notice that when a do-script fails, it leaves it's artifact in an inconsistent state.
It'd be better if, when a do-script fails, the artifact is left alone; only when the do-script executes successfully is the artifact updated.
Making sure every do-script does this right is boring and error-prone.

If none of the dependencies of an artifact change, there's no point in running it's do-script again: it should produce the same results.
There's just the question of figuring out which dependencies have or have not changed.
Unfortunately, that's easy to mess up: not only do you have to keep track of direct dependencies, but also transitive dependencies.
And you know what, isn't the do-script itself a dependency, as are all the places where we searched for a do-script.
On top of that, you still have to write the change-detection test.

Look, I just want to type the name of an artifact and get it automatically built according to my scripts.
Let's write a little utility that can do all the boring stuff:

  * find the script
  * check dependencies
  * run the script
  * record the dependencies
  * atomically update the output file

Instead of invoking a do-script directly, let's instead always invoke it through our little utility.

That's why dbj came up with [redo](https://cr.yp.to/redo.html).
Admittedly, he only came up with the idea, not the implementation, so there are some known weaknesses, and some he admits himself.
`zedo` implements the strong points of redo, while addressing the weakness of `redo` while gaining the strengths of redo.
It is named differently only so that there is no confusion between the two interfaces.

## Architectural Notes

The static architecture of `zedo` is not difficult; it's the dynamic architecture (flow of information during execution) that is tricky.
We'll come back to the static architecture, but it will only make sense after examining the dynamics.

### Dynamic Architecture

Commonly, invoking a program involves only one OS process (also called—slightly wrongly—the program).
However, `zedo` is designed to be able to indirectly call `zedo` again.
This forms a tree of `zedo` invocations, and it is this tree as a whole that I think it makes sense to refer to when someone "runs `zedo`".
In the subsequent documentation, we will refer to "invocation" and "invocation tree" to the exclusion of "process", "program", "call", or similarly ambiguous terms.

  * An invocation is a single OS process executing the `zedo` program.
  * An invocation tree is a set of invocations arranged according to parent-child process relationships.
    The trick here is that a child in the invocation tree is not the direct child in terms of OS processes; rather, a child in the tree is the OS child of a do-script which is itself an OS child of the parent invocation.

In addition to invocation trees and and invocations, there is an additional level of execution which relates to when `zedo` is called with many arguments.
In particular `zedo <condition> <a> <b...>` is functionally equivalent to `zedo <condition> <a>; zedo <condition> <b...>`, where `<condition>` is one of `always`, `ifchange`, or `ifcreate`.
Obviously the former incurs less overhead by starting fewer OS processes.
The equivalency does mean that we can segregate different regions of independent execution within an invocation into what I call "atoms".

In fact, the atom-invocation equivalences mean that the parent-child relationships in the invocation tree are not between invocations.
Instead, a parent is an atom, and a child is an invocation.
Thanks to the equivalency, though, it won't often matter if we use the less-precise notion of parentage between invocations.



Within each level of execution, there are invariants: tree invariants, invocation invariants, and atomic invariants.
Within an OS process, it is easy to maintain these invariants: simply hold them in an immutable data structure once that level of execution has been built.
Maintaining these invariants over the tree is more touchy, as we only have access to environment variables.
We reserve environment variables beginning in `ZEDO__` for internal use by `zedo`.
Almost all of these are setup by the root invocation before any child invocations are created, and are not mutated again by any invocation in the tree.
Each new invocation can read these tree invariants from the environment to set up its own data structures.

However, because the root and child invocations are executing the same `zedo` program, each invocation must first determine its position in the invocation tree.
If we could pass a `--parent` argument to `zedo`, that would be a very functional solution, but unfortunately, `zedo` does not directly invoke its children—they are invoked by a user's do-script, and we don't want to make the user pass this boring, error-prone flag all the time.
Therefore, we have to resort to environment variables to inform a newly-invoked process of its position in the tree.
The compromise it not as bad as it seems: we would need to determine an invocation's location in the tree anyway in order to record dependency information.
We just have to be careful to pick up an atomic invariant stored in an agreed-upon environment variable and move it (not copy) it to an invocation invariant, and to always set up that same environment variable with new parentage data before there is the potential to creating a new invocation.

