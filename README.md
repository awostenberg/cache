Cache
====


###n-way set associative cache exploration

2 level cache, with pluggable cache vacate strategy, in the functional style.

For examples you can run interactively in F# interactive (FSI), 
see file [script.fsx](https://github.com/awostenberg/cache/blob/master/cache/Script.fsx).

Here's an example you can try from FSI or http://tryfs.net/

    #load "Cache.fs"
    open Cache.Realistic

    let square n =
      printfn "computing square of %d" n
      n*n
      
    let memSquare = memoizeWithNWayCache square {nshelves=1;nbooks=10} Policy.lru 
    List.map memSquare [1..10]   // computes squares - see printfs
    List.map memSquare [1..10]   // 2nd time hits cached - no printfs
    memSquare 55                 // vacates oldest
    memSquare 1                  // should recalculate 