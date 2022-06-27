#!/bin/sh

echo "Starting NEUROEVO Server......"
read VER
echo "Please enter neuroevo or all"
read result
if [ $result = neuroevo ]; then
    ./neuroevo-${VER}/bin/neuroevo daemon
elif [ $result = all ]; then
    ./neuroevo-${VER}/bin/neuroevo daemon
else
    echo "wrong input!"
fi
