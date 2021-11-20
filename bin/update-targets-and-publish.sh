#!/bin/bash

git pull -X theirs

bash bin/render-targets.sh > render.log 2>&1 &

bash bin/make-parallel-targets.sh > targets.log 2>&1 &

bash bin/update-remote.sh > git-remote.log 2>&1 &
