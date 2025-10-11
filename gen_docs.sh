#!/usr/bin/env sh

stack exec -- haddock --hyperlinked-source --html \
    src/Lessons/Lesson01.hs \
    src/Lessons/Lesson02.hs \
    src/Lessons/Lesson03.hs