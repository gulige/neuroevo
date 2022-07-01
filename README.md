neuroevo
=====

An OTP application

Build
-----

    $ rebar3 compile

Run
-----

1. renew flow
polis:sync(). % recompile and load all the modules
polis:reset(). % create the mnesia database
polis:start(). % start the polis process
population_monitor:start().
visor:start().

2. continue flow
polis:start().
population_monitor:continue(test).
......
population_monitor:stop().
......
population_monitor:continue(test).
......

3. benchmark flow
polis:reset().
polis:start().
benchmarker:start(test).

4. visor
visor:start().
visor:zo().
......
visor:stop().
......
visor:start().
......

