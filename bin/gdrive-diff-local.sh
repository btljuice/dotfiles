#!/bin/bash

for i in `find . -type f ! -path './.gd/*'`
do
    if [ -f "$i" ]
    then
        drive diff -base-local=true -skip-content-check=true -recursive=false $i
    fi
done
