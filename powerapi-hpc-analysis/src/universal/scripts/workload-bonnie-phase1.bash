#!/bin/bash

# Has to be run in sudo mode

bonnie -u powerapi &>/dev/null &
sleep 10

rm -rf Bonnie.*
ps -ef | grep bonnie | grep -v grep | awk '{print $2}' | xargs kill -9 &>/dev/null

exit 0
