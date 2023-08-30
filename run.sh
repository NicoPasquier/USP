#!/bin/bash

cd USP-cbls

command="runMain USP.Test"

for var in $@
do
    command="${command} ${var}"
done 

sbt "$command"