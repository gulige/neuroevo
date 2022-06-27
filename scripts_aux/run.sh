#!/bin/sh

echo "Starting NEUROEVO Server......"
read VER
echo "Please enter neuroevo or all"
read result
if [ $result = neuroevo ]; then
    gnome-terminal --title="neuroevo" -- ./neuroevo-${VER}/bin/neuroevo console
elif [ $result = all ]; then
    gnome-terminal --title="neuroevo" -- ./neuroevo-${VER}/bin/neuroevo console
else
    echo "wrong input!"
fi
