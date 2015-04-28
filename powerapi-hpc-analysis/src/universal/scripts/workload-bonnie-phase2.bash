#!/bin/bash

# Has to be run un sudo mode

bonnie -u powerapi

rm -rf Bonnie.*

ps -ef | grep bonnie | grep -v grep | awk '{print $2}' | xargs kill -9 &>/dev/null

exit 0
