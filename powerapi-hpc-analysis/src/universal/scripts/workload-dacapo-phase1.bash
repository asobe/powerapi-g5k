#!/bin/bash

# Has to be run in sudo mode

cd $1

java -jar dacapo-9.12-bach.jar $2 -s large  &>/dev/null &

sleep 10

ps -ef | grep "java -jar dacapo" | grep -v grep | awk '{print $2}' | xargs kill -9 &>/dev/null

exit 0
