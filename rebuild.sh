#!/bin/bash

while true; do
  stack build --force-dirty #  && timeout 120 .stack-work/install/x86_64-linux/lts-12.7/8.4.3/bin/impress
  sleep 20 #  0.4
done
