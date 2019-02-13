#!/bin/sh
echo "starting neuroevo server......"
rebar3 release -n neuroevo
_build/default/rel/neuroevo/bin/neuroevo start
sleep 3
_build/default/rel/neuroevo/bin/neuroevo attach
