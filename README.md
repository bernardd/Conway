Conway
======

## Conway's Game of Life

#### Running:

Compilation is pretty trivial:

    erlc *.erl

The UI, such as it is, is run entirely within the erlang shell. First, initialise the cell store:

    > cell_store:init().
    cell_store

Now individual cells can be set to live thus:

    > cell_store:set_cell({5, 5}, true).
    true

That's a bit cumbersome, though, so there's a set of pre made common life forms in the life_forms module. You can add them like this:

    > life_forms:add(life_forms:glider(), {30, 50}).
    ok

That will add a glider with top-left coordinates of 30,50.
You can also transform the pre-set life forms using the mirror_x, mirror_y, rotate_l and rotate_r functions (to flip horizontaly, flip vertically, rotate counter-clockwise 90 degrees and rotate clockwise 90 degrees respectively):

    > life_forms:add( life_forms:rotate_r( life_forms:mirror_x( life_forms:loaf() )), {10, 10}).
    ok

Running the simulation can be done a step at a time:

    > conway:run_step().
    ok

To display the 100x100 grid starting at 0,0, just call

    > printer:print().
    <Grid>
    ok

You can specify a particular rectangle to print like this:

    > printer:print({10,10}, {20, 20}).
    <11x11 grid>
    ok

To run repeated iterations at 400ms intervals, printing out the 100x100 grid at each step you can use:

    > test:run().
    ....

This will keep running until you press RETURN/ENTER.

Persistant storage is provided by:

    > cell_store:save().
    ok

and

    > cell_store:load().
    ok

(Note that for the sake of simplicity this will always save to/load from a file called 'cell_store' in the current working directory).

Finally, the test module also contains a couple of example starting states, one with three different life forms and one with four gliders. To use those start states, run:

    > test:setup_example().

or

    > test:setup_gliders().

respectively. test:run/0 can then be used as described above.


#### Limitations/Notes:

* The field is effectively unlimited in size due to Erlang's arbitrary precision integers - at least until you run out of memory. (I realise the spec was technically to support 2^64 x 2^64, but I figured removing that limit would not be a big problem - I can add it in if it's a scrict rather than minimum requirement).
* There's no protection on the cell storage - someone could theoretically save/load it while the simulation is running and get an inconsistant state.
* There's no real concurrency (even though we're working with Erlang) - there's a few ways to approach concurrency in CGoL but they all complicate the implementation significantly, and the request was for a "simple, minimalist" solution :)
* Even ignoring ways to parallelise it, there's a couple of possibilities for optimisation that spring to mind, particularly around the calculation of cells that require processing, but again those would add complexity.
* Saving/loading always goes to/from the same file name in the current working directory.
* cell_store is just a very thin wrapper around ets to make it relatively easy to swap in a different store to test performance.
* In the interests of simplicity, in true erlang style there's basically no error handling (even where a proper Erlang app would have it, like file access).
* The persistant store is just a simple DETS dump of the ETS table, and is far from optimal size-wise.
* I've lazily done -compile(export_all) on every module to make testing easier. Obviously that's generally not good form in real apps.
