neuroevo
=====

A perfect neuroevolution platform, which was formed by refactoring the source code in Chapter 18 of
the book HandbookOfNeuroevolutionThroughErlang written by Gene I. Sher.

All source code in this project excluding the gs copied from otp_src_19.1 follow
the License provided by the original author Gene I. Sher.

Please pay attention to the following License explanation:

This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM

Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
All rights reserved.

This code is licensed under the version 3 of the GNU General Public License.
Please see the LICENSE file that accompanies this project for the terms of use.


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

