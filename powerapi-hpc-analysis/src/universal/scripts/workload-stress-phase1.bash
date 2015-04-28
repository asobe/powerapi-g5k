#!/bin/bash

# Has to be run in sudo mode

stress -c 2 &>/dev/null &
ppid=$!
read -a pids <<< $(ps --ppid ${ppid} ho pid)

taskset -cp 0 ${pids[0]} &>/dev/null
taskset -cp 4 ${pids[1]} &>/dev/null

sleep 10

ps -ef | grep stress | grep -v grep | awk '{print $2}' | xargs kill -9 &>/dev/null

exit 0
