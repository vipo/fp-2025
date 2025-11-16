#!/usr/bin/env sh

stack exec -- haddock \
    --hyperlinked-source \
    --quickjump \
    --html \
    src/Lessons/Lesson01.hs \
    src/Lessons/Lesson02.hs \
    src/Lessons/Lesson03.hs \
    src/Lessons/Lesson04.hs \
    src/Lessons/Lesson06.hs \
    src/Lessons/Lesson07.hs \
    src/Lessons/Lesson08.hs \
    src/Lessons/Lesson09.hs
