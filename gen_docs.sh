#!/usr/bin/env sh

stack exec -- haddock \
    --hyperlinked-source \
    --html \
    --source-base=URL \
    src/Lessons/Lesson01.hs \
    src/Lessons/Lesson02.hs \
    src/Lessons/Lesson03.hs \
    src/Lessons/Lesson04.hs \
