#!/bin/sh

MUTE=`amixer sget Master | grep -E -o -m1 '(\[off\])|(\[on\])'`
if [ "$MUTE" == "[off]" ];
then
    echo 'mute'
else
    amixer sget Master | grep -E -m 1 -o '[0-9]+%'
fi
