# Zedo

Rebuild target files when dependencies change.

The software* you want to build may be very complex.
An automatic build system is just another piece of software, and should be treated as such.
If it isn't very complex, then a few build scripts should take care of it.
If it does get complex though, you'll want the comfort of a full scripting language to do with as you see fit.
`zedo` gains its power by not taking anything in your project for granted; it merely coordinates your build scripts.

*It doesn't need to be software, it could be anything stored digitally --- a blog, music, the results of scientific analysis, &c. That said, you'll be a little more comfortable writing build scripts if you're already a programmer.

If you are familiar with `redo`, `zedo` is based on that, but does not attempt to be backwards-compatible with DJB's original discussions.
Now that `redo` has been experimented with in practice, a few limitations of the system have been identified.
My goal with zedo is not to follow DJB's words as scripture, but to create the best tool I can, and that means altering the interface to correct the subtle design errors.
