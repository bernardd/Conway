Conway
======

Conway's game of life

Limitations/Notes:

* No protection on Cells table - someone could theoretically save/load it while the simulation is running and get an inconsistant state.
* No concurrency (even though we're working with Erlang) - there's a few ways to approach concurrency in CGoL but they all complicate the implementation significantly, and the request was for a "simple, minimalist" solution :)
* There's a couple of opportunites for optimisation that spring to mind, particularly around the calculation of cells that require processing, but again those would introduce complexity.
* Saving/loading always goes to/from the same file name in the current working directory
* cell_store is just a very thin wrapper around ets to make it easy to swap in a different store to test performance
