#!/bin/bash

git pull -X theirs

bash bin/render-targets.sh > render.log 2>&1

bash bin/make-parallel-targets.sh > targets.log 2>&1

bash bin/update-git-remote.sh > git-remote.log 2>&1

bash bin/update-targets-remote.sh > targets-remote.log 2>&1
