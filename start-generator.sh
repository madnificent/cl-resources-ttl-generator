#!/bin/bash

echo "Ensuring output folder exists"
mkdir -p /config/output

echo "Loading configuration from base image"
/load-config.sh

echo "Starting conversion"
sbcl --load /usr/src/startup.lisp
