#!/bin/sh

amixer sget Master | grep -E -m 1 -o '[0-9]+%'
