neuroevo
=====

An OTP application

Build
-----

    $ rebar3 compile

Run
-----

polis:sync(). % recompile and load all the modules
polis:reset(). % create the mnesia database
polis:start(). % start the polis process
population_monitor:start(). or benchmarker:start(test).
visor:start().

polis:start().
population_monitor:continue(test).
population_monitor:stop().
