#!/bin/zsh
PROJ_PATH='/Users/mthieu/Repos/jeopardizing'
cd ${PROJ_PATH}/ignore/narration
mdls renamed/*/*.mp3 | grep 'FSName\|DurationSeconds' > durations.txt
