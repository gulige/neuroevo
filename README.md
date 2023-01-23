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

<img width="551" alt="图片" src="https://user-images.githubusercontent.com/3211537/177258025-85d261a2-7ee6-4ed4-88a2-21f570d93d99.png">

Build
-----

    $ rebar3 release -n neuroevo

Run
-----

Assuming user id is 100 (integer),

polis:sync(). % recompile and load all the modules  
polis:reset(). % create the mnesia database  
polis:start(100). % start the polis process  
population_monitor:start(100).  
visor:start(100).
