#!/bin/bash

cd USP-cbls

sbt "runMain USP.Test $1 $2"