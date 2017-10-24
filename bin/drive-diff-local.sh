#!/bin/bash

for i in *
do
    if [ -f "$i" ]
    then
        drive diff -base-local=true -skip-content-check=true -recursive=false $i
    fi
done
