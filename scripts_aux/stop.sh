#!/bin/sh

echo "Stopping NEUROEVO Server......"
read VER
echo "Please enter neuroevo or all"
read result
if [ $result = neuroevo ]; then
    ./neuroevo-${VER}/bin/neuroevo stop
elif [ $result = all ]; then
    ./neuroevo-${VER}/bin/neuroevo stop
else
    echo "wrong input!"
fi
