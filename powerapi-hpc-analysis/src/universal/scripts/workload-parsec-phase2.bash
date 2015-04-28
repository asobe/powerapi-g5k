#!/bin/bash

# Has to be run in sudo mode

cd $1
source env.sh
parsecmgmt -a run -i native -p $2 &>/dev/null

ps -ef | grep inst/ | grep -v grep | awk '{print $2}' | xargs kill -9 &>/dev/null

exit 0
